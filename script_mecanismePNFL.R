
data <-  read_xlsx("Book1.xlsx", sheet = "fig6")


processedData <- data |> 
    select(`Régénération Naturelle`:last_col()) |> 
    pivot_longer(cols = everything(), names_to = "autresApproches", values_to = "n") |> 
    group_by(autresApproches) |> 
    summarise(
      N = sum(n)
    ) |> 
    mutate(
      percent = N/sum(N) * 100
    )

library(tidyr)
library(plotly)


plot_ly(processedData) |>
  add_segments(x = ~0, xend = ~percent, y = ~autresApproches, yend = ~autresApproches,
               line = list(color = "black")) |>
  add_markers(x = ~percent, y = ~autresApproches, 
              marker = list(color = "blue", size = 8)) |>
  layout(
    title = " ",
    xaxis = list(title = "", ticksuffix = "%", 
                 showgrid = T,
                 zeroline = TRUE,
                 zerolinecolor = "black",
                 zerolinewidth = 2),
    yaxis = list(title = "", showgrid = FALSE),
    showlegend = FALSE,
    font = list(size = 15, color = "black")
  )
