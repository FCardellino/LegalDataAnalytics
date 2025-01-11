# Descripción y fundamentación de la app    
El objetivo de esta app es mostrar una versión funcional de cómo se puede aplicar la metodología y herramientas de process mining al análisis de datos legales (en este caso datos de procesos judiciales)
La app consiste en un tablero con diferentes métricas y gráficos que permiten obtener un entendimiento rápido y visual de algunas variables de los procesos judiciales como por ejemplo:    
- Cantidad de actos procesales
- Distribución de casos según diferentes categorías de análisis    
- Medidas estadísticas como tiempos promedios de duración de los casos    
- Visualización del flujo, "velocidad/demora" y cuellos de botella que se generan mientras un casos se lleva adelante
    
La app fue desarrollada utilizando principalmente la libreria **bupaR** para el armado de las funcionalidades orientadas al minado de procesos y **shiny** para el front.        
Los datos observados son de causas judiciales correspondientes al fuero Civil y Comercial, iniciadas en 2022 en los tribunales de la provincia de Córdoba.

# Documentación    
Estos son los links a las librerias utilizadas y a la fuente de la cual se obtuvieron los datos judiciales:
- [bupaR](https://bupar.net/)
- [shiny](https://shiny.posit.co/)
- [Portal de Datos Abiertos del Ministerio de Justicia Argentino](https://datos.jus.gob.ar/tr/dataset/poderes-judiciales-causas-no-penales)

### Link a la app
En el siguiente enlace se puede ver una versión funcionando del código de GitHub:
- [Process Mining en Procesos Judiciales de Córdoba - Fuero Civ y Com](https://fndcardellino-lda.shinyapps.io/shinyapplda_lpm/)

