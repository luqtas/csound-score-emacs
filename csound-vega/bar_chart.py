import altair as alt
import pandas as pd
import sys
import json
import os

if len(sys.argv) < 3:
    print("Error: Missing required arguments (path, x_name).")
    sys.exit(1)

data_path = sys.argv[1]
x_name = sys.argv[2]

with open(data_path, 'r') as f:
    df = pd.DataFrame(json.load(f))

# Altair handles the frequency calculation automatically with 'count()'
chart = alt.Chart(df).mark_bar().encode(
    x=alt.X('value:O', title=x_name),
    y=alt.Y('count():Q', title='occurrence')
).properties(
    title=f'Distribution of {x_name}'
)

output_html = "/tmp/csound_bar.html"
chart.save(output_html)
os.system(f"firefox {output_html}")
