



library(ggplot2)
library(emojifont)
library(scales)
library(hexSticker)

load.fontawesome()


icon <- ggplot() +
  geom_text(aes(x = 1, y = 1, label = fontawesome('fa-file-text')),
            family = "fontawesome-webfont",
            size = 90) +
  theme_void() + theme_transparent()



# s <- sticker(last_plot(), package="CTUtemplate",
#              p_size=18,
#              p_family = "Arial",
#              s_x=1, s_y=1, s_width=1.3, s_height=1,
#              filename="sticker.png",
#              h_fill = CTUtemplate::unibeRed(.3),
#              h_color = CTUtemplate::unibeRed()
#              , spotlight = FALSE,
#              l_x = 1,
#              l_y = 1.7,
#              l_width = 5,
#              l_height = 5,
#              l_alpha = .5,
#              )


s <- sticker(icon, package="",
             s_x=1, s_y=1.15, s_width=2, s_height=2,
             filename="man/figures/sticker.png",
             h_fill = CTUtemplate::unibeRed(.3),
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "CTUtemplate",
             u_size = 12,
             u_x = 1,
             u_y = 0.15
)
s
