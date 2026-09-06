import pandas as pd
import numpy as np
import geopandas as gpd
import xml.etree.ElementTree as ET
from shapely.geometry import Point

reefs = gpd.read_file("C:/Users/tanvi/OneDrive/Documents/GitHub/OysterProject/Data/reefs.shp")  #load the siteshapefile
site_mapping = {'WSR1': 'WSR', 'WSR2': 'WSR', 'WSR3': 'WSR'}   #convert all WSR subsites into 1 main site 
reefs['Reefs'] = reefs['Reefs'].replace(site_mapping)

release_files = {
    'LH': "C:/Users/tanvi/OysterbayHDmodel/MESH_Tanvi/Mesh Generator/OB_CSH_model_inputs.m21fm - Result Files/LH_PT_JULY14.xml",
    'OBC': "C:/Users/tanvi/OysterbayHDmodel/MESH_Tanvi/Mesh Generator/OB_CSH_model_inputs.m21fm - Result Files/OBC_PT_JULY14.xml",
    'WSR': "C:/Users/tanvi/OysterbayHDmodel/MESH_Tanvi/Mesh Generator/OB_CSH_model_inputs.m21fm - Result Files/WSR_PT_JULY14.xml"
}  #load the xml files from particle tracking

buffer = 0.5        # settlement distance tolerance
Max_current_speed = 1.0       # max current speed allowed at the moment of settlement
all_reefs = ['LH', 'OBC', 'WSR', 'CovePoint', 'CSH']  #define all the reef names

reefs_buffered = reefs.copy()
reefs_buffered['geometry'] = reefs_buffered.geometry.buffer(buffer) #add a buffer of 0.5m around the reefs

#function to read xml track files and store in a df format
def parse_particle_track_xml(filepath, chunk_size=100000):
    records = []
    chunks = []
    current_timestep = None
    
    context = ET.iterparse(filepath, events=("start", "end")) #read line by line instead loading the full file
    context = iter(context)
    event, root = next(context)

    for event, elem in context:
        tag = elem.tag

        if event == "start" and tag == "TimeStep": 
            current_timestep = int(elem.attrib["nr"]) 

        elif event == "end" and tag == "Particle":
            row = {
                "timestep": current_timestep,
                "particle_id": int(elem.attrib["Nr"]), # record timestep and particle number
            }

            for child in elem:
                if child.text is not None:
                    row[child.tag] = float(child.text) #grab particle coordinates, depth, hspeed 
                else:
                    row[child.tag]= np.nan 
            
            records.append(row)
            
            # Clear the element from memory
            elem.clear()
            root.clear()

            # Process in chunks to avoid memory overload
            if len(records) >= chunk_size:
                chunks.append(pd.DataFrame.from_records(records))
                records = []

        elif event == "end" and tag == "TimeStep":
            elem.clear()
            root.clear()

    # Combine all the smaller chunks into one final DataFrame
    final_df = pd.concat(chunks, ignore_index=True)
    return final_df

#function to find the first instance of a particle falling within a reef polygon when horizontal current speed is below 1m/s
def find_settlements(final_df, reefs_buffered, Max_current_speed = 1.0):

    particles_gdf = gpd.GeoDataFrame(final_df, geometry=gpd.points_from_xy(final_df["x"], final_df["y"]), crs=reefs_buffered.crs)#convert into a spatial database

    inside_reef = gpd.sjoin(particles_gdf, reefs_buffered[["Reefs", "geometry"]], how="inner", predicate="within") #conduct spatial join

    #keep only rows where the current wasn't too strong to settle
    candidates = inside_reef[inside_reef["_hspeed"] < Max_current_speed]

    # For each particle, keep just its first qualifying row
    settlements = (
        candidates
        .sort_values(["particle_id", "timestep"]) 
        .groupby("particle_id")
        .first() #after sorting and grouping by particle id, grab the first row
        .reset_index()[["particle_id", "timestep", "Reefs"]]
        .rename(columns={"Reefs": "settlement_site"})
    )
    return settlements


#run the functions
origin_sites = ['LH', 'OBC', 'WSR']
connectivity_counts = pd.DataFrame(0, index= origin_sites, columns=all_reefs)

for origin_site, filepath in release_files.items():
    final_df = parse_particle_track_xml(filepath)
    settlements = find_settlements(final_df, reefs_buffered, Max_current_speed)

    counts_by_destination = settlements["settlement_site"].value_counts()
  
    for destination_site, count in counts_by_destination.items():
        connectivity_counts.loc[origin_site, destination_site] = count

connectivity_proportions = connectivity_counts / 1000

print("\nConnectivity counts:")
print(connectivity_counts)

print("\nConnectivity proportions:")
print(connectivity_proportions)
