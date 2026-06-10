library(hexSticker)
library(rsvg)
library(showtext)
font_add_google("Josefin Sans","jsans")

showtext_auto()

sticker("devel/logo.svg", package="TBL.NOW", p_size=50,
        p_y = 0.6, p_color = "#D9D9D9",
        s_x=1, s_y=1.2, s_width=0.6, s_height=0.6, h_fill = "#262626",
        p_family = "jsans", p_fontface = "bold", h_color = "#B85348", h_size = 2,
        filename="inst/figures/hex.png", dpi = 750, lineheight = 0.1)

