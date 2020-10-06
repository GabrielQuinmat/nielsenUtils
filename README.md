![](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Nielsen_Logo.svg/1280px-Nielsen_Logo.svg.png)

# Funciones utilitarias Nielsen GDDC Brasil

Este repositorio engloba un conjunto de funciones y variables que suelen utilizarse dentro de las actividades diarias.
Algunas funciones sólo facilitan hacer tareas sencillas (y el uso de las mismas se repite a lo largo de varios programas en GDDC Brasil) y procuran no repetir código en distintos scripts.

Un ejemplo es la función (y posiblemente la más usada) **read.dt**. Esta función toma como primer argumento la ruta a un archivo, leerlo y castearlo a un objeto data.table. Los tipos de archivos que puede leer son TXT, CSV, EXCEL, SAS7BDAT, FEATHER y RDS. Son los tipos de archivos más comunes y también tiene un proceso integrado para estandarizar nombres de columnas, así como una limpieza de caracteres dentro de las columnas Strings.

 Nota: Este repositorio no contiene información sensible. Por lo que no hay problema con compartir el repositorio entre compañeros de Nielsen.

# Instalacion

Esta paquetería vive dentro de un ambiente de BitBucket, por lo que para poder instalarlo primero se debe tener una cuenta en la plataforma de Bitbucket Nielsen. Segundo, se debe tener permisos de lectura del repositorio. Posterior a eso, es tan sencillo instalarlo como:

```r
devtools::install_bitbucket("https://adlm.nielsen.com/bitbucket/scm/~quya9001/nielsenutils.git")
```

# Uso

Todas las funciones tienen documentación y se puede navegar dentro de ellas en la descripción de la paquetería.

Un ejemplo es el siguiente código, ejecutado en consola:

```r
# Para ver la descripción formal interna de la paquetería. Esto incluye con quién
# se puede comunicar de encontrar errores. Desde este punto se puede navegar al índice
# de la paquetería, donde se listan todas las funciones disponibles.
?`nielsenUtils-package`


# También se puede ver la documentación diercta como en cualquier otra paquetería
?read.dt
```

# Dependencias

Las siguientes paqueterías son necesarias para poder utilizar todas las funciones:

- purrr
- data.table
- glue
- stringr
- futile.logger
- haven
- openxlsx
- furrr
- bit64
- magrittr
- feather
- glue
- R (>= 3.6)