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

# Stacked bar chart: x is value, y is count, color is instrument
chart = alt.Chart(df).mark_bar().encode(
    x=alt.X('value:O', title=x_name),
    y=alt.Y('count():Q', title='occurrence', stack=None),
    color='instrument:N',  # This creates the stacking by instrument
    tooltip=['instrument', 'count()']
).properties(
    title=f'Overlapping Distribution of {x_name} by Instrument'
).interactive()

output_html = "/tmp/csound_stacked.html"
chart.save(output_html)
os.system(f"firefox {output_html}")
