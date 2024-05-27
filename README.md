# lab2_212547662_VicenteAninat

Cada uno de los predicados se llama con su nombre y los argumentos que necesita, ejemplo station( 1, "Metro ejemplo", t, 30, ST0), ST0 es para tener una lista de los elementos de una estacion para operar con esta en otros predicados, por ejemplo section( ST0, ST1, 2, 50, S0) donde ST1 es otra estacion cualquiera y S0 es una lista con los elementos del section.

Para los tipos de estación se tienen solo c, m, r y t que son combinación, mantención, regular y terminal respectivamente, y para los tipos de estación se tienen ct y tr, central y terminal respectivamente, estos son los únicos tipos admitidos.

Para hacer varias consultas concatenadas, estas deben estar separadas por comas y todas las consultas deben terminar con un punto final.

Es importante mencionar que los requerimientos funcionales, desde expresar una red de metro en formato string (subwayToString) en adelante, no se encuentran implementadas, por lo que se recomienda abstenerse de consultarlas. Además, que los predicados que requieren un nombre de estación, si el nombre de la estación no coincide con ningún nombre de las estaciones de una línea, el programa fallará.

Finalmente, para el uso del sistema en general consultar el archivo main donde se cargan todos los archivos necesarios para el funcionamiento, puesto que usar el sistema en el archivo de un TDA puede no entregar los resultados esperados. Esto para tener una estructura más ordenada de todo el desarrollo.
