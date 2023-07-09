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
from mpl_toolkits.mplot3d import Axes3D
from sklearn.preprocessing import LabelEncoder

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
    # initialize matplotlib plot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection="3d")
    # and use offsets to make sure the images fit the plot
    
    offset = max_image_size // 2
    image_centers_area_size = plot_size - 2 * offset

    tsne_plot = 255 * np.ones((plot_size, plot_size, plot_size, 3), np.uint8)

    # now we'll put a small copy of every image to its corresponding T-SNE coordinate
    for image_path, label, x, y, z in tqdm(
            zip(images, labels, tx, ty, tz),
            desc='Building the T-SNE plot',
            total=len(images)):
        
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
        
        # Convert the class color to the appropriate format (0-1 range)
        color = [channel / 255 for channel in colors_per_class[label]]
        
        # Add the image point to the 3D plot
        ax.scatter([x], [y], [z], c=[color], label=label)

        # Display the image at the corresponding T-SNE coordinates
        ax.text(x, y, z, "", zorder=1, fontsize=8)
        ax.imshow(image, extent=(tl_x, br_x, tl_y, br_y), zorder=0)

    # Set the plot limits based on the range of T-SNE coordinates
    ax.set_xlim(np.min(tx), np.max(tx))
    ax.set_ylim(np.min(ty), np.max(ty))
    ax.set_zlim(np.min(tz), np.max(tz))

    # Add legend and labels
    ax.legend(loc='best')
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')

    
    # rotate the plot
    #tsne_plot = np.rot90(tsne_plot, k=1, axes=(0, 2))
    # save the plot as image
    #plt.imsave("/workspace/tsne.png",tsne_plot[:, :, ::-1])
    # show the plot
    plt.imshow(tsne_plot[:, :, :, ::-1])
    plt.show(block=True)

import plotly.graph_objects as go


def visualize_tsne_points(tx, ty, tz, labels):
    # initialize matplotlib plot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection="3d")

    # for every class, we'll add a scatter plot separately
    for label in colors_per_class:
        # find the samples of the current class in the data
        indices = [i for i, l in enumerate(labels) if l == label]

        # extract the coordinates of the points of this class only
        current_tx = np.take(tx, indices)
        current_ty = np.take(ty, indices)
        current_tz = np.take(tz, indices)

        # convert the class color to matplotlib format:
        # BGR -> RGB, divide by 255, convert to np.array
        color = np.array([colors_per_class[label][::-1]], dtype=np.float64) / 255

        # add a scatter plot with the correponding color and label
        ax.scatter(current_tx, current_ty, current_tz, c=color, label=label)

    # build a legend using the labels we set previously
    ax.legend(loc='best')
 
    # finally, show the plot
    plt.savefig('tsne-points.png')
    plt.show()


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
    visualize_tsne_points(tx, ty,tz, labels)

    # visualize the plot: samples as images
    visualize_tsne_images(tx, ty,tz, images, labels, plot_size=plot_size, max_image_size=max_image_size)


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

    tsne = TSNE(n_components=3).fit_transform(features)

    visualize_tsne(tsne, image_paths, labels)

if __name__ == '__main__':
    main()