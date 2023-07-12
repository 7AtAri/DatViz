import argparse
from tqdm import tqdm
import cv2
import torch
import random
import numpy as np
from sklearn.manifold import TSNE
import matplotlib.pyplot as plt

from animals_dataset import AnimalsDataset, collate_skip_empty, colors_per_class
from resnet import ResNet101
#from mpl_toolkits.mplot3d import Axes3D
from sklearn.preprocessing import LabelEncoder
import plotly.graph_objects as go
import ntpath
import re
import base64
from PIL import Image
import io
from io import BytesIO
from dash import Dash, dcc, html, Input, Output, no_update, callback

#import os
#import urllib.request


def fix_random_seeds():
    seed = 10
    random.seed(seed)
    torch.manual_seed(seed)
    np.random.seed(seed)


def get_features(dataset, batch, num_images):
    # move the input and model to GPU for speed if available
    if torch.cuda.is_available():
        device = 'cuda'
    else:
        device = 'cpu'

    # initialize our implementation of ResNet
    model = ResNet101(pretrained=True)
    model.eval()
    model.to(device)

    # read the dataset and initialize the data loader
    dataset = AnimalsDataset(dataset, num_images)
    dataloader = torch.utils.data.DataLoader(dataset, batch_size=batch, collate_fn=collate_skip_empty, shuffle=True)

    # we'll store the features as NumPy array of size num_images x feature_size
    features = None

    # we'll also store the image labels and paths to visualize them later
    labels = []
    image_paths = [] 

    for batch in tqdm(dataloader, desc='Running the model inference'):
        images = batch['image'].to(device)
        labels += batch['label']
        image_paths += batch['image_path']

        with torch.no_grad():
            output = model.forward(images)

        current_features = output.cpu().numpy()
        if features is not None:
            features = np.concatenate((features, current_features))
        else:
            features = current_features

    return features, labels, image_paths


# scale and move the coordinates so they fit [0; 1] range
def scale_to_01_range(x):
    # compute the distribution range
    value_range = (np.max(x) - np.min(x))

    # move the distribution so that it starts from zero
    # by extracting the minimal value from all its values
    starts_from_zero = x - np.min(x)

    # make the distribution fit [0; 1] by dividing by its range
    return starts_from_zero / value_range


def scale_image(image, max_image_size):
    image_height, image_width, _ = image.shape

    scale = max(1, image_width / max_image_size, image_height / max_image_size)
    image_width = int(image_width / scale)
    image_height = int(image_height / scale)

    image = cv2.resize(image, (image_width, image_height))
    return image


def draw_rectangle_by_class(image, label):
    image_height, image_width, _ = image.shape

    # get the color corresponding to image class
    color = colors_per_class[label]
    image = cv2.rectangle(image, (0, 0), (image_width - 1, image_height - 1), color=color, thickness=3)

    return image


def compute_plot_coordinates(image, x, y, z, image_centers_area_size, offset):
    image_height, image_width, _ = image.shape

    # compute the image center coordinates on the plot
    center_x = int(image_centers_area_size * x) + offset

    # in matplotlib, the y axis is directed upward
    # to have the same here, we need to mirror the y coordinate
    center_y = int(image_centers_area_size * (1 - y)) + offset
    
    center_z = int(image_centers_area_size * z)

    # knowing the image center, compute the coordinates of the top left and bottom right corner
    tl_x = center_x - int(image_width / 2)
    tl_y = center_y - int(image_height / 2)
    tl_z = center_z 

    br_x = tl_x + image_width
    br_y = tl_y + image_height
    br_z = tl_z
    

    #print("tl_z:", tl_z)
    #print("br_z:", br_z)

    return tl_x, tl_y, tl_z, br_x, br_y, br_z


def visualize_tsne_images(tx, ty, tz, images, labels, plot_size=2000, max_image_size=100):
    # we'll put the image centers in the central area of the plot
    # and use offsets to make sure the images fit the plot
    offset = max_image_size // 2
    image_centers_area_size = plot_size - 2 * offset

    tsne_plot = 255 * np.ones((plot_size, plot_size, plot_size, 3), np.uint8)

    # now we'll put a small copy of every image to its corresponding T-SNE coordinate
    for image_path, label, x, y, z in tqdm(
            zip(images, labels, tx, ty, tz),
            desc='Building the T-SNE plot',
            total=len(images)
    ):
        image = cv2.imread(image_path)
        
        # to debug:
        #print(image.shape)

        # scale the image to put it to the plot
        image = scale_image(image, max_image_size)

        # draw a rectangle with a color corresponding to the image class
        image = draw_rectangle_by_class(image, label)

        # compute the coordinates of the image on the scaled plot visualization
        tl_x, tl_y, tl_z, br_x, br_y, br_z = compute_plot_coordinates(image, x, y, z, image_centers_area_size, offset)

        # put the image to its TSNE coordinates using numpy subarray indices
        tsne_plot[tl_y:br_y, tl_x:br_x, tl_z,:] = image
        #tsne_plot[tl_y:br_y, tl_x:br_x, tl_z:br_z, 0] = image[:, :, 0]  # Red channel
        #tsne_plot[tl_y:br_y, tl_x:br_x, tl_z:br_z, 1] = image[:, :, 1]  # Green channel
        #tsne_plot[tl_y:br_y, tl_x:br_x, tl_z:br_z, 2] = image[:, :, 2]  # Blue channel

    
    # rotate the plot
    #tsne_plot = np.rot90(tsne_plot, k=1, axes=(0, 2))
    # save the plot as image
    plt.imsave("/workspace/tsne.png",tsne_plot[:, :, :, ::-1])
    # show the plot
    plt.imshow(tsne_plot[:, :, :, ::-1])
    plt.show()

def visualize_tsne_images_points_dash(tx, ty, tz, images, labels, plot_size=2000, max_image_size=100):
    # Create a 3D scatter plot
    label_encoder = LabelEncoder()
    label_encoder.fit(labels)
    numeric_labels = label_encoder.transform(labels)

    # Extract the desired part of the string using regular expressions
    image_labels = [re.search(r'([^/]+)\.jpg$', image).group(1) for image in images]

    # Encode images as base64 strings
    encoded_images = []
    for image_path in images:
        with Image.open(image_path) as img:
            img = img.resize((max_image_size, max_image_size))
            buffered = io.BytesIO()
            img.save(buffered, format="jpeg")
            encoded_image = base64.b64encode(buffered.getvalue()).decode("utf-8")
            im_url= "data:image/jpeg;base64, " + encoded_image


    fig = go.Figure(data=[go.Scatter3d(
        x=tx,
        y=ty,
        z=tz,
        mode='markers',
        marker=dict(
            size=5,
            color=numeric_labels,  # Color points based on labels
            colorscale='Viridis',
            opacity=0.8
        )
    )])
    
    fig.update_traces(hoverinfo="none",
                      hovertemplate=None)
                        
    # Customize the plot as desired
    fig.update_layout(
        title='T-SNE Visualization',
        scene=dict(
            xaxis_title='X',
            yaxis_title='Y',
            zaxis_title='Z'
        )
    )

    # Save the plot as an HTML file
    #fig.write_html('/workspace/tsne_plot.html')

    # Initialize Dash app
    app = Dash(__name__)

    app.layout = html.Div(
        className="container",
        children=[
        dcc.Graph(id='tsne-graph', figure=fig, clear_on_unhover=True),
        dcc.Tooltip(id="tsne_graph-tooltip", direction='bottom'),
    ],)
    
    @callback(
    Output("tsne_graph-tooltip", "show"),
    Output("tsne_graph-tooltip", "bbox"),
    Output("tsne_graph-tooltip", "children"),
    Input("tsne-graph", "hoverData"),
)

    def display_hover_info(hoverData):
        if hoverData is None:
            return html.Div()

        point_index = hoverData['points'][0]['pointIndex']
        bbox = hover_data["bbox"]
        num = hover_data["pointNumber"]
        image_label = image_labels[point_index]

        children = [
        html.Div([
            html.Img(
                src=im_url,
                style={"width": "50px", 'display': 'block', 'margin': '0 auto'},
            ),
            html.P(" " + str(image_label[num]), style={'font-weight': 'bold'})
                ])
        ]

        return True, bbox, children

    # Run the app
    app.run(debug=True)


    
def visualize_tsne_images_points_go(tx, ty, tz, images, labels, plot_size=2000, max_image_size=100):
    # Create a 3D scatter plot
    label_encoder = LabelEncoder()
    label_encoder.fit(labels)
    numeric_labels = label_encoder.transform(labels)
    
    # Extract PetID part of the path using regular expressions
    image_labels = [re.search(r'([^/]+)\.jpg$', image).group(1) for image in images]

    # Encode images as base64 strings
    encoded_images = []
    for image_path in images:
        with Image.open(image_path) as img:
            img = img.resize((max_image_size, max_image_size))
            buffered = BytesIO()
            img.save(buffered, format="JPEG")
            encoded_image = base64.b64encode(buffered.getvalue()).decode("utf-8")
            encoded_images.append(encoded_image)
     
     
    label_colors = { label: color  # Replace `color` with the desired color for each label
                    for label, color in zip(set(labels), ['#f76060','#3f8ded'])}
    colors = [label_colors[label] for label in labels]

    
    fig = go.Figure(data=[go.Scatter3d(
        x=tx,
        y=ty,
        z=tz,
        mode='markers',
        text=images,  # Set the text for hover-over information
        hovertext=images,  # Set the hover text to be the same as the text
        hovertemplate="<b>PetID-ImageNr:</b> %{customdata}<br>",  # Custom hover information
        customdata=image_labels,  # Use the extracted PetID as custom data
        marker=dict(
            size=5,
            color=colors,  # Color points based on labels and colorscheme
            #colorscale='Viridis',
            opacity=0.5
        )
    )])

    # Customize the plot as desired
    fig.update_layout(
        title='T-SNE 3d Visualization of Neural Network features',
        width=1000, # adjusts plot size
        height=1000, # adjusts plot size
        scene=dict( 
        xaxis=dict(visible=False), # makes axis vanish
        yaxis=dict(visible=False),
        zaxis=dict(visible=False))
    )
    # Disable trace labels in hover information
    fig.update_traces(hoverlabel=dict(namelength=0))

    # Save the plot as an HTML file
    fig.write_html('/workspace/tsne_plot_hover.html')
    


def visualize_tsne(tsne, images, labels, plot_size=2000, max_image_size=100):
    # extract x and y coordinates representing the positions of the images on T-SNE plot
    tx = tsne[:, 0]
    ty = tsne[:, 1]
    tz = tsne[:, 2]
    
    #print("tz: ", tz)

    # scale and move the coordinates so they fit [0; 1] range
    tx = scale_to_01_range(tx)
    ty = scale_to_01_range(ty)
    tz = scale_to_01_range(tz)
    #print("tz_scaled: ", tz)

    # visualize the plot: samples as colored points
    #visualize_tsne_points(tx, ty,tz, labels)

    # visualize the plot: samples as images
    visualize_tsne_images_points_go(tx, ty,tz, images, labels, plot_size=plot_size, max_image_size=max_image_size)
    #visualize_tsne_images_points_dash(tx, ty,tz, images, labels, plot_size=plot_size, max_image_size=max_image_size)


def main():
    
    parser = argparse.ArgumentParser()
    parser.add_argument('--path', type=str, default='data/raw-img')
    parser.add_argument('--batch', type=int, default=64)
    parser.add_argument('--num_images', type=int, default=500)
    args = parser.parse_args()

    fix_random_seeds()

    features, labels, image_paths = get_features(
        dataset=args.path,
        batch=args.batch,
        num_images=args.num_images
    )
    
    #dataset_path = '/workspace/cats_and_dogs'  # Specify the dataset path as a string
    #batch_size = 64  # Specify the batch size as a numeric value
    #num_images = 500  # Specify the number of images as a numeric value

    #features, labels, image_paths = get_features(dataset_path, batch_size, num_images)

    tsne = TSNE(n_components=3).fit_transform(features)

    visualize_tsne(tsne, image_paths, labels)

if __name__ == '__main__':
    main()