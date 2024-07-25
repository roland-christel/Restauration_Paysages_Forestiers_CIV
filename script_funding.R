


data <-  read_xlsx("Book1.xlsx", sheet = "funding")

processedData <- data |> 
    select(starts_with("Financement")) |> 
    pivot_longer(cols = starts_with("Financement") ,
                 names_to = "typeFunding", values_to = "Values") |> 
    group_by(typeFunding) |> 
    summarise(
      total = sum(Values)
    ) |> 
    mutate(
      percent = total/sum(total) * 100
    ) 
    
colors <- pal_uchicago("default")(length(unique(processedData$typeFunding)))

plot_ly(processedData, 
        labels = ~paste(typeFunding,"\n", sprintf("%.1f%%", percent)), 
        values = ~percent, 
        type = 'pie', 
        hole = 0.65,
        marker = list(colors = colors),
        textfont = list(
          color = "#1B1919FF",
          size = 15),
        textinfo = 'label',
        insidetextorientation = 'radial') %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         showlegend = FALSE)

#===============================================================================


data <-  read_xlsx("Book1.xlsx", sheet = "funding")


processedData <- data |> 
  select("catMoney", starts_with("Financement")) |> 
  na.omit() |> 
  filter(catMoney != "Fonds propres") |> 
  mutate(
    catMoney = factor(catMoney, levels=c("Moins de 20 millions","De 20 à 50 millions",
                                         "De 50 à 100 millions", "Plus de 100 millions"))
  ) |> 
  pivot_longer(cols = starts_with("Financement") ,
               names_to = "typeFunding", values_to = "Values") |> 
  group_by(typeFunding, catMoney) |> 
  summarise(
    total = sum(Values)
  ) |> 
  group_by(catMoney)|> 
  mutate(
    percent = total/sum(total) * 100
  ) 

plot_ly(processedData, 
        x = ~catMoney, 
        y = ~percent, 
        type = 'bar', 
        name = ~typeFunding, 
        color = ~typeFunding,
        colors = colors,
        maker= list(colour = "black")
        ) %>%
  layout(barmode = 'stack',
         showlegend = TRUE,
         xaxis = list(title = 'Moyen financier alloué aux projets',
                      titlefont = list(
                        color = "#1B1919FF",
                        size = 15)),
         yaxis = list(title = '',ticksuffix = '%'),
         legend = list(font = list(color = "#1B1919FF", size = 12))
         
         )

