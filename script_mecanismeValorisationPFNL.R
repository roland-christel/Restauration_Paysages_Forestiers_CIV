

data <-  read_xlsx("Book1.xlsx", sheet = "fig7")

data  |>  
      select(`Propriété et Utilisation des Produits` : last_col()) |> 
      pivot_longer(cols = everything(), names_to = "mecanismes", values_to = "n") |> 
      group_by(mecanismes) |> 
      summarise(
       N = sum(n)
      ) |> 
      mutate(
        percent = N/sum(N) * 100
      ) |> 
 plot_ly() |>
  add_segments(x = ~0, xend = ~percent, y = ~mecanismes, yend = ~mecanismes,
               line = list(color = "black")) |>
  add_markers(x = ~percent, y = ~mecanismes, 
              marker = list(color = "blue", size = 8)) |>
  layout(
    title = " ",
    xaxis = list(title = "", ticksuffix = "%", 
                 showgrid = T,
                 zeroline = TRUE,
                 zerolinecolor = "black",
                 zerolinewidth = 2),
    yaxis = list(title = "",
                 showgrid = FALSE,
                  automargin = TRUE, ticklen = 1),
    showlegend = FALSE,
    font = list(size = 15, color = "black")
  )
