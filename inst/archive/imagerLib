tmp <- 
  imager::load.image(
    file.path("C:/Users/JALLEN/OneDrive - Environmental Protection Agency (EPA)/workspace",
              "Datagui1.PNG"))

plot(tmp)

Width = knitr::opts_chunk$get('fig.width')
Height = knitr::opts_chunk$get('fig.height')
vec.res = 300

WidthP <- Width * vec.res
HeightP <- Height * vec.res

tmpW <- imager::width(tmp)
tmpH <- imager::height(tmp)

tmp1 <-
  imager::resize(tmp, size_x = as.integer(-(tmpW/WidthP * 100)), size_y = as.integer(-(tmpH/HeightP * 100)))

plot(tmp1)

imager::save.image(tmp1,
  file.path("C:/Users/JALLEN/OneDrive - Environmental Protection Agency (EPA)/workspace",
            "Datagui1_.png"))

plot.return <- pander::evals('tmp1', res = vec.res,
              width = WidthP, height = HeightP)[[1]]$result
plot(plot.return)
