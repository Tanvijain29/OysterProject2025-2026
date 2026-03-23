import pandas as pd
import geopandas as gpd
import xml.etree.ElementTree as ET
from shapely.geometry import Point

reefs = gpd.read_file("C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/reefs.shp")
site_mapping = {'WSR1': 'WSR', 'WSR2': 'WSR', 'WSR3': 'WSR'}
reefs['Reefs'] = reefs['Reefs'].replace(site_mapping)
all_reefs = ['LH', 'OBC', 'WSR', 'CovePoint', 'CSH']

release_files = {
    'LH': "C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/ParticletrackLH.xml",
    'OBC': "C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/ParticletrackOBC.xml",
    'WSR': "C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/ParticletrackWSR.xml"
}

connectivity_matrix = pd.DataFrame(index=all_reefs, columns=all_reefs)

# --- 3. LOOP THROUGH EACH RELEASE FILE ---
for source_site, file_path in release_files.items():
    print(f"Processing release from {source_site}...")
    
    tree = ET.parse(file_path)
    root = tree.getroot()
    particle_data = {}
    
    # Parse the XML to get final coordinates
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
    
    # Perform the spatial join
    settled_particles = gpd.sjoin(particles_gdf, reefs, how="inner", predicate="within")
    
    # Count the settlement and force all reefs to appear
    counts = settled_particles['Reefs'].value_counts().reindex(all_reefs, fill_value=0)
    # Add this completed row to our master connectivity matrix
    total_released = len(particle_data)
    connectivity_matrix.loc[source_site] = counts/total_released

# --- 4. DISPLAY THE FINAL MATRIX ---
print("\n--- FINAL CONNECTIVITY MATRIX (RAW COUNTS) ---")
print(connectivity_matrix)