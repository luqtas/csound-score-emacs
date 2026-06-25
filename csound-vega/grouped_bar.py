import altair as alt
import pandas as pd
import sys
import json
import os

if len(sys.argv) < 3:
    print("Error: Missing required arguments (path, value_name).")
    sys.exit(1)

data_path = sys.argv[1]
value_name = sys.argv[2]

with open(data_path, 'r') as f:
    df = pd.DataFrame(json.load(f))

# Organized like Vega example:
# Main X-axis categories are Instruments.
# Inside each instrument, bars are grouped side-by-side by Value.
chart = alt.Chart(df).mark_bar().encode(
    x=alt.X('instrument:N', title=''),
    y=alt.Y('count():Q', title='occurrence'),
    color=alt.Color('value:O', title=value_name),
    xOffset='value:O',  # Spreads the values side-by-side within the Instrument section
    tooltip=['instrument', 'value', 'count()']
).properties(
    title=f'Value Distribution Grouped by Instrument ({value_name})',
    width=600,
    height=400
).interactive()

output_html = "/tmp/csound_grouped.html"
chart.save(output_html)
os.system(f"firefox {output_html}")
