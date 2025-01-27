## This file creates vfunc.png, the hex sticker.  To create file
## function_adding_motivational_example.png, open
## function_adding_motivational_example.svg (which is held under
## version control) in inkscape and export to png format.



library("hexSticker")

sticker("function_adding_motivational_example.png",
        package = "vfunc",
        p_size = 36,
        s_x = 1, s_y = 0.75,
        s_width = 2,
        asp = sqrt(3)/2,
        white_around_sticker = TRUE,
        h_fill = "#7733FF",
        h_color = "#000000",
        filename = "vfunc.png")

