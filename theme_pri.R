theme_pri <- function(){
  theme(
    # 1. Border
    panel.border = element_rect(fill = NA, color = "black", linetype = "solid", size = 1),
    # 2. Background
    panel.background = element_rect(fill = NA),
    # 3. Grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # 4. Text
    text = element_text(color = "black", size = 10, family = "serif"),
    # 5. Axis 
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    # axis.text = element_text(),
    # axis.title = element_text(),
    # 6. Legend 
    legend.title = element_blank(),
    legend.position = "bottom"
  )
}