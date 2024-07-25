data <-  read_xlsx("Book1.xlsx", sheet = "fig5")


dataProcessed <- data |> 
     select(Teck:last_col()) |> 
     na.omit() |> 
     pivot_longer(cols = everything() , 
                  names_to = "sp",
                  values_to = "n") |> 
     group_by(sp) |> 
     summarise(
       N = sum(n)
     ) |> 
     mutate(percent = N/sum(N)*100)|>
    arrange(desc(percent)) |> 
    mutate(sp = factor(sp, levels = sp))

# Créer le graphique avec plotly
plot_ly(dataProcessed, 
             x = ~percent, 
             y = ~sp, 
             type = 'bar', 
             orientation = 'h',
             marker = list(color = 'steelblue')) |>
  layout(
    title = "Pourcentage par espèce",
    xaxis = list(
      title = "Pourcentage",
      tickformat = ".1f",  # Format à un décimal
      ticksuffix = "%",    # Ajouter le symbole % après chaque tick
      showgrid = FALSE
    ),
    yaxis = list(title = ""),
    margin = list(l = 150),  # Augmenter la marge gauche pour les étiquettes
    hoverlabel = list(bgcolor = "white"),
    hovermode = "y"
  )


dynamic_height <- max(600, length(unique(dataProcessed$sp)) * 20)

plot_ly(dataProcessed, 
        x = ~percent, 
        y = ~sp, 
        type = 'bar', 
        orientation = 'h',
        marker = list(color = 'steelblue')) |>
  layout(
    title = "Pourcentage par espèce",
    xaxis = list(
      title = " ",
      tickformat = ".1f",  # Format à un décimal
      ticksuffix = "%",    # Ajouter le symbole % après chaque tick
      showgrid = TRUE,zeroline = TRUE,     # Afficher la ligne du zéro
      zerolinecolor = '#969696',
      zerolinewidth = 1
    ),
    yaxis = list(title = "", autorange = "reversed"),
    margin = list(l = 150, r = 50, b = 50, t = 50),  # Ajuster les marges
    hoverlabel = list(bgcolor = "white"),
    hovermode = "y",
    height = dynamic_height
  )
