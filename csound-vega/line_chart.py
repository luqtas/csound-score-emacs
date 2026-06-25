import altair as alt
import pandas as pd
import sys
import json
import os

if len(sys.argv) < 4:
    print("Error: Missing required arguments (path, x_name, y_name).")
    sys.exit(1)

data_path = sys.argv[1]
x_name = sys.argv[2]
y_name = sys.argv[3]

with open(data_path, 'r') as f:
    df = pd.DataFrame(json.load(f))

chart = alt.Chart(df).mark_line(point=True).encode(
    x=alt.X('x:Q', title=x_name),
    y=alt.Y('y:Q', title=y_name),
    color='instrument:N',
    tooltip=['instrument', 'x', 'y']
).properties(
    title=f'{y_name} vs {x_name}',
    width=600,
    height=400
).interactive()

output_html = "/tmp/csound_line.html"
chart.save(output_html)
os.system(f"firefox {output_html}")
