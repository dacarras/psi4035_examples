
# Componente para realizar inferencias entre personas

Vik (2014), en el capitulo 20, esta creando un centroide para separar las inferencias de interés de cada pregunta. Para crear estos centroides, emplea una formula como la siguiente (ecuación 18.3, p224):

$$W_{1} = \frac{\lambda_{1}T_{1}+\lambda_{2}T_{2}}{\sqrt{\lambda_{1}+\lambda_{2}}}$$

---


Lambda 1 ($$\lambda_{1}$$) y lambda 2 ( $$\lambda_{2}$$) los define con pesos 1 y -1, de modo tal que el centro de ambos puntajes, la distancia común entre ambos puntos, le permite al autor realizar inferencias sobre las diferencias **entre** personas. Adicionalmente, divide, o estandariza estos pesos por la raiz cuadrada de la suma de estos pesos, de modo tal, que el componente generado siga estando en una *métrica* de los puntajes originales.

---

La diferencia con el ejercio del capitulo 18, es que en el capitulo 20, Vik llama a este componente "$$W_{o}$$". De esta forma, re expresaremos la ecuación anterior, según como es implementado en el capitulo 20.

$$W_{0} = \frac{\lambda_{1}T_{1}+\lambda_{2}T_{2}}{\sqrt{\lambda_{1}+\lambda_{2}}}$$

Donde,

- $$\lambda_{1}$$ =  1
- $$\lambda_{2}$$ = -1
