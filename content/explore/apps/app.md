+++
# A Projects section created with the Portfolio widget.
widget = "portfolio"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 65  # Order that this section will appear.

title = "Applications"
subtitle = ""

[content]
  # Page type to display. E.g. project.
  # page_type = "shinyapp"
  
  # Filter toolbar (optional).
  # Add or remove as many filters (`[[content.filter_button]]` instances) as you like.
  # To show all items, set `tag` to "*".
  # To filter by a specific tag, set `tag` to an existing tag name.
  # To remove toolbar, delete/comment all instances of `[[content.filter_button]]` below.
  
  # Default filter index (e.g. 0 corresponds to the first `[[filter_button]]` instance below).
  filter_default = 0
  
  
[design]
  # Choose how many columns the section has. Valid values: 1 or 2.
  columns = "2"

  # Toggle between the various page layout types.
  #   1 = List
  #   2 = Compact
  #   3 = Card
  #   5 = Showcase
  view = 2

  # For Showcase view, flip alternate rows?
  flip_alt_rows = false

[design.background]
  # Apply a background color, gradient, or image.
  #   Uncomment (by removing `#`) an option to apply it.
  #   Choose a light or dark text color by setting `text_color_light`.
  #   Any HTML color name or Hex value is valid.
  
  # Background color.
  # color = "navy"
  
  # Background gradient.
  # gradient_start = "DeepSkyBlue"
  # gradient_end = "SkyBlue"
  
  # Background image.
  # image = "background.jpg"  # Name of image in `static/media/`.
  # image_darken = 0.6  # Darken the image? Range 0-1 where 0 is transparent and 1 is opaque.

  # Text color (true=light or false=dark).
  # text_color_light = true  
  
[advanced]
 # Custom CSS. 
 css_style = ""
 
 # CSS class.
 css_class = ""
+++

<h3>
<a href="http://52.24.141.166:3838/power_analysis/" target="_blank">Power Analysis</a>
</h3>
Analyzes power of your experiment under a variety of conditions

<h3>
<a href="http://52.24.141.166:3838/power_simulation/" target="_blank">Power Simulation</a>
</h3>
Simulate power of meta-analyses under a variety of conditions

<h3>
<a href="http://52.24.141.166:3838/visualization/" target="_blank">Visualization</a>
</h3>
Explore a variety of interactive charts driven by the MetaLab database by your datasets and moderators

<h3>
<a href="http://52.24.141.166:3838/data_validation/" target="_blank">Validation</a>
</h3>
Validate that new datasets are ready for inclusion in the MetaLab database
