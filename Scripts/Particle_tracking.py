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

AGE_MIN = 14 * 86400   # seconds, start of competency window
AGE_MAX = 21 * 86400   # seconds, end of competency window

connectivity_matrix = pd.DataFrame(0.0, index=all_reefs, columns=all_reefs)

for source_site, file_path in release_files.items():
    tree = ET.parse(file_path)
    root = tree.getroot()

    settled = {}
    all_pids = set()

    for timestep in root.findall('.//TimeStep'):
        for particle_class in timestep.findall('ParticleClass'):
            class_id = particle_class.get('id')

            rows = []
            for p in particle_class.findall('Particle'):
                pid = (class_id, p.get('Nr'))
                all_pids.add(pid)

                age = float(p.find('_age').text)
                if age < AGE_MIN or age > AGE_MAX:
                    continue
                if pid in settled:
                    continue

                x = float(p.find('x').text)
                y = float(p.find('y').text)
                rows.append({'pid': pid, 'geometry': Point(x, y)})

            if not rows:
                continue

            step_gdf = gpd.GeoDataFrame(rows, crs=reefs.crs)
            hits = gpd.sjoin(step_gdf, reefs, how="inner", predicate="within")
            hits = hits[~hits.index.duplicated(keep='first')]

            for _, row in hits.iterrows():
                settled[row['pid']] = row['Reefs']

    counts = pd.Series(list(settled.values())).value_counts().reindex(all_reefs, fill_value=0)
    total_released = len(all_pids)
    connectivity_matrix.loc[source_site] = counts / total_released

print(connectivity_matrix)