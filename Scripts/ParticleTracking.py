import pandas as pd
import geopandas as gpd
import xml.etree.ElementTree as ET
from shapely.geometry import Point

reefs = gpd.read_file("C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/reefs.shp")
site_mapping = {'WSR1': 'WSR', 'WSR2': 'WSR', 'WSR3': 'WSR'}
reefs['Reefs'] = reefs['Reefs'].replace(site_mapping)
all_reefs = ['LH', 'OBC', 'WSR', 'CovePoint', 'CSH']

release_files = {
    'LH': "C:/Users/tanvi/OysterbayHDmodel/MESH_Tanvi/Mesh Generator/MODEL RUN/ParticleTrackLH.xml",
    'OBC': "C:/Users/tanvi/OysterbayHDmodel/MESH_Tanvi/Mesh Generator/MODEL RUN/ParticleTrackOBC.xml",
    'WSR': "C:/Users/tanvi/OysterbayHDmodel/MESH_Tanvi/Mesh Generator/MODEL RUN/ParticletrackWSR.xml"
}

connectivity_matrix = pd.DataFrame(index=all_reefs, columns=all_reefs)

for source_site, file_path in release_files.items():

    tree = ET.parse(file_path)
    root = tree.getroot()
    particle_data = {}
    
    # get final coordinates
    for timestep in root.findall('.//TimeStep'):
        for particle in timestep.findall('.//Particle'):
            pid = particle.get('Nr')
            x_coord = float(particle.find('x').text)
            y_coord = float(particle.find('y').text)
            particle_data[pid] = {'x': x_coord, 'y': y_coord}
            
    # Convert to spatial points
    geometry = [Point(data['x'], data['y']) for data in particle_data.values()]
    particle_df = pd.DataFrame(particle_data.values())
    particles_gdf = gpd.GeoDataFrame(particle_df, geometry=geometry, crs=reefs.crs)
    
    # spatial join
    settled_particles = gpd.sjoin(particles_gdf, reefs, how="inner", predicate="within")
    
    # Count the settlement 
    counts = settled_particles['Reefs'].value_counts().reindex(all_reefs, fill_value=0)

    # Add to the main connectivity matrix
    total_released = len(particle_data)
    connectivity_matrix.loc[source_site] = counts/total_released


print(connectivity_matrix)

import geopandas as gpd

shapefile_path = "C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/reefs.shp"
reefs = gpd.read_file(shapefile_path)
reefs = reefs.to_crs(epsg=32618)

site_mapping = {'WSR1': 'WSR', 'WSR2': 'WSR', 'WSR3': 'WSR'}
reefs['Reefs'] = reefs['Reefs'].replace(site_mapping)

# Calculate the area in square meters for every polygon
reefs['Area_sqm'] = reefs.geometry.area

# Sum the areas by the specific reef name
total_areas = reefs.groupby('Reefs')['Area_sqm'].sum()

# Multiply by  carrying capacity  
k_values_50 = total_areas * 50

print(total_areas.round(2))

print(k_values_50.round(0).astype(int))


