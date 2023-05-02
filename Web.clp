;;;==============================================================================
;;;   WEBMO sistema experto                                                     |
;;;                                                                             |
;;;     El sistema experto WEBMO tiene como objetivo                            |
;;;     ayudar a los desarrolladores de software en la                          |
;;;     selección de herramientas adecuadaspara diferentes proyectos y en el    |
;;;     ámbito del desarrollo web. El sistema experto utilizará                 |
;;;     los requisitos específicos del proyecto y las características           |
;;;     de las herramientas disponibles para ofrecer recomendaciones            |
;;;     personalizadas y precisas para cada caso en particular.                 |
;;;                                                                             |
;;;     CLIPS Version 6.3                                                       |
;;;                                                                             |
;;;     Para ejecutar,load, reset, and run.                                     |
;;;     preguntas de yes or no.                                                 |
;;;==============================================================================

;;;***************************
;;;* DEFFUNCTIONS DEFINITIONS *
;;;***************************

(deffunction propagate-goal ""
   (goal is ?goal)
   (rule (if ?variable $?)
         (then ?goal ? ?value))
   =>
   (assert (goal is ?variable)))

(deffunction remove-rule-no-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ~?value $?))
   =>
   (retract ?f))

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE 
       else FALSE))


;;;**************************
;;;* INFERENCE ENGINE RULES *
;;;**************************


(defrule determinar-desarrollo ""
   (not (herramienta ?))
   =>
   (if (yes-or-no-p "Desea recomendaciones de herramientas de desarrollo web (yes/no)? ") 
       then 
       (if (yes-or-no-p "Esta buscando una herramienta para el front-end (yes/no)? ")
           then (assert (herramienta "Herramientas para el front-end: React, Angular, Vue.js"))
           else (if (yes-or-no-p "Esta buscando una herramienta para el back-end (yes/no)? ") 
            then(assert (herramienta "Herramientas para el back-end: Node.js, Django, Ruby on Rails"))
            else (assert (herramienta "Para herramientas de desarrollo web, es importante considerar las necesidades especificas del proyecto y las habilidades del equipo de desarrollo. Algunas opciones populares incluyen Visual Studio Code, Atom, Sublime Text y Eclipse."))))
       else 
       (assert (herramienta "Si no estas buscando recomendaciones de herramientas de desarrollo web, ¿en que puedo ayudarte?")))
       
       
    (if (yes-or-no-p "Desea recomendaciones de lenguajes y frameworks del lado del servidor (yes/no)? ") 
       then 
       (if (yes-or-no-p "Prefiere PHP (yes/no)? ")
           then (assert (herramienta "PHP y Laravel"))
           else (if (yes-or-no-p "Prefiere Python (yes/no)? ") 
                    then (assert (herramienta "Python, Django o Flask"))
                    else (if (yes-or-no-p "Prefiere Ruby (yes/no)? ") 
                             then (assert (herramienta "Ruby y Ruby on Rails"))
                             else (if (yes-or-no-p "Prefiere JavaScript (yes/no)? ") 
                                      then (assert (herramienta "Express"))
                                      else (if (yes-or-no-p "Prefiere Java (yes/no)? ") 
                                               then (assert (herramienta "Spring"))
                                               else (if (yes-or-no-p "Prefiere C# (yes/no)? ") 
                                                        then (assert (herramienta "ASP.NET"))
                                                        else (assert (herramienta "No se especificó ninguna preferencia, se recomienda evaluar las necesidades del proyecto y las habilidades del equipo de desarrollo para elegir el lenguaje y framework adecuados."))))))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de lenguajes y frameworks del lado del servidor, ¿en qué puedo ayudarte?")))   

     (if (yes-or-no-p "Desea recomendaciones de tecnologías básicas para el desarrollo web (yes/no)? ") 
       then 
       (if (yes-or-no-p "Esta buscando una tecnología para estructura básica de página web (yes/no)? ")
           then (assert (herramienta "HTML"))
           else (if (yes-or-no-p "Esta buscando una tecnología para estilos y diseño personalizado (yes/no)? ") 
                    then (assert (herramienta "CSS"))
                    else (if (yes-or-no-p "Esta buscando una tecnología para interactividad y animaciones (yes/no)? ") 
                             then (assert (herramienta "JavaScript"))
                             else (assert (herramienta "Para tecnologías básicas de desarrollo web, es importante considerar las necesidades específicas del proyecto y las habilidades del equipo de desarrollo. Las tecnologías principales son HTML, CSS y JavaScript.")))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de tecnologías básicas para el desarrollo web, ¿en qué puedo ayudarte?")))

    
    (if (yes-or-no-p "Desea recomendaciones de lenguajes y frameworks del lado del cliente (yes/no)? ") 
       then 
       (if (yes-or-no-p "Esta buscando un enfoque basado en componentes (yes/no)? ")
           then (assert (herramienta "React o Vue.js"))
           else (if (yes-or-no-p "Esta buscando un enfoque basado en MVC (yes/no)? ") 
                    then (assert (herramienta "AngularJS"))
                    else (if (yes-or-no-p "Esta buscando un lenguaje con tipado estático (yes/no)? ") 
                             then (assert (herramienta "TypeScript"))
                             else (assert (herramienta "Para lenguajes y frameworks del lado del cliente, es importante considerar las necesidades específicas del proyecto y las habilidades del equipo de desarrollo. Algunas opciones populares incluyen React, Vue.js, AngularJS y TypeScript.")))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de lenguajes y frameworks del lado del cliente, ¿en qué puedo ayudarte?")))


    (if (yes-or-no-p "Desea recomendaciones de frameworks de diseño web (yes/no)? ") 
       then 
       (if (yes-or-no-p "Esta buscando un framework responsivo con componentes prediseñados (yes/no)? ")
           then (assert (herramienta "Bootstrap"))
           else (if (yes-or-no-p "Esta buscando un framework basado en Material Design (yes/no)? ") 
                    then (assert (herramienta "Materialize"))
                    else (if (yes-or-no-p "Esta buscando un framework personalizado y adaptable (yes/no)? ") 
                             then (assert (herramienta "Foundation"))
                             else (if (yes-or-no-p "Esta buscando un framework con utilidades de estilo pre-diseñadas para diseños personalizados (yes/no)? ") 
                                      then (assert (herramienta "Tailwind CSS"))
                                      else (if (yes-or-no-p "Esta buscando un framework centrado en la claridad y la concisión (yes/no)? ") 
                                               then (assert (herramienta "Semantic UI"))
                                               else (assert (herramienta "Para frameworks de diseño web, es importante considerar las necesidades específicas del proyecto y las habilidades del equipo de desarrollo. Algunas opciones populares incluyen Bootstrap, Materialize, Foundation, Tailwind CSS y Semantic UI.")))))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de frameworks de diseño web, ¿en qué puedo ayudarte?")))
    
    
     (if (yes-or-no-p "Desea recomendaciones de APIs (yes/no)? ") 
       then 
       (if (yes-or-no-p "Esta buscando una API versátil y compatible con cualquier lenguaje y plataforma (yes/no)? ")
           then (assert (herramienta "RESTful API"))
           else (if (yes-or-no-p "Esta buscando una API para consulta de datos para múltiples fuentes de datos (yes/no)? ") 
                    then (assert (herramienta "GraphQL"))
                    else (if (yes-or-no-p "Esta buscando una API para entornos empresariales y aplicaciones de alta seguridad (yes/no)? ") 
                             then (assert (herramienta "SOAP"))
                             else (if (yes-or-no-p "Esta buscando una API para aplicaciones en tiempo real con comunicación bidireccional (yes/no)? ") 
                                      then (assert (herramienta "WebSocket API"))
                                      else (if (yes-or-no-p "Esta buscando una API para autenticación sin contraseñas (yes/no)? ") 
                                               then (assert (herramienta "OAuth"))
                                               else (if (yes-or-no-p "Esta buscando una API para integración de mapas interactivos (yes/no)? ") 
                                                        then (assert (herramienta "Google Maps API"))
                                                        else (if (yes-or-no-p "Esta buscando una API para integración de pagos en línea (yes/no)? ") 
                                                                 then (assert (herramienta "Stripe API"))
                                                                 else (assert (herramienta "Para APIs, es importante considerar las necesidades específicas del proyecto y las habilidades del equipo de desarrollo. Algunas opciones populares incluyen RESTful API, GraphQL, SOAP, WebSocket API, OAuth, Google Maps API y Stripe API.")))))))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de APIs, ¿en qué puedo ayudarte?")))

       
    (if (yes-or-no-p "Desea recomendaciones de herramientas de diseño y prototipado (yes/no)? ") 
       then 
       (if (yes-or-no-p "Busca una herramienta multiplataforma (yes/no)? ")
           then (assert (herramienta "Adobe XD"))
           else (if (yes-or-no-p "Busca una herramienta basada en la nube y colaborativa (yes/no)? ") 
                    then (assert (herramienta "Figma"))
                    else (if (yes-or-no-p "Busca una herramienta de diseño vectorial para Mac (yes/no)? ") 
                             then (assert (herramienta "Sketch"))
                             else (if (yes-or-no-p "Busca una herramienta de diseño y colaboración en equipo con prototipos interactivos (yes/no)? ") 
                                      then (assert (herramienta "InVision"))
                                      else (if (yes-or-no-p "Busca una herramienta de diseño gráfico fácil de usar en línea (yes/no)? ") 
                                               then (assert (herramienta "Canva"))
                                               else (assert (herramienta "No se especificó ninguna preferencia, se recomienda evaluar las necesidades del proyecto y las habilidades del equipo de diseño para elegir la herramienta adecuada.")))))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de herramientas de diseño y prototipado, ¿en qué puedo ayudarte?")))   
       
    (if (yes-or-no-p "Desea recomendaciones de arquitecturas de aplicaciones web (yes/no)? ") 
       then 
       (if (yes-or-no-p "Busca una arquitectura Modelo-Vista-Controlador (MVC) (yes/no)? ")
           then (assert (herramienta "Modelo-Vista-Controlador (MVC)"))
           else (if (yes-or-no-p "Busca una Arquitectura orientada a servicios (SOA) (yes/no)? ") 
                    then (assert (herramienta "Arquitectura orientada a servicios (SOA)"))
                    else (if (yes-or-no-p "Busca una arquitectura basada en microservicios (yes/no)? ") 
                             then (assert (herramienta "Arquitectura basada en microservicios"))
                             else (if (yes-or-no-p "Busca una arquitectura basada en eventos (yes/no)? ") 
                                      then (assert (herramienta "Arquitectura basada en eventos"))
                                      else (if (yes-or-no-p "Busca una arquitectura cliente-servidor (yes/no)? ") 
                                               then (assert (herramienta "Arquitectura cliente-servidor"))
                                               else (assert (herramienta "No se especificó ninguna preferencia, se recomienda evaluar las necesidades del proyecto y las habilidades del equipo de desarrollo para elegir la arquitectura adecuada.")))))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de arquitecturas de aplicaciones web, ¿en qué puedo ayudarte?")))   



    (if (yes-or-no-p "Desea recomendaciones de bases de datos (yes/no)? ") 
       then 
       (if (yes-or-no-p "Busca una base de datos relacional de código abierto con escalabilidad y alta disponibilidad (yes/no)? ")
           then (assert (herramienta "MySQL"))
           else (if (yes-or-no-p "Busca una base de datos relacional con características avanzadas y soporte para transacciones ACID (yes/no)? ") 
                    then (assert (herramienta "PostgreSQL"))
                    else (if (yes-or-no-p "Busca una base de datos NoSQL escalable y altamente disponible para datos no estructurados (yes/no)? ") 
                             then (assert (herramienta "MongoDB"))
                             else (if (yes-or-no-p "Busca una plataforma de desarrollo con base de datos en tiempo real y sistema de autenticación (yes/no)? ") 
                                      then (assert (herramienta "Firebase"))
                                      else (if (yes-or-no-p "Busca una base de datos relacional para aplicaciones empresariales con alta escalabilidad y seguridad (yes/no)? ") 
                                               then (assert (herramienta "Oracle"))
                                               else (if (yes-or-no-p "Busca una base de datos relacional para aplicaciones web empresariales con soporte para transacciones ACID (yes/no)? ") 
                                                        then (assert (herramienta "SQL Server"))
                                                        else (assert (herramienta "No se especificó ninguna preferencia, se recomienda evaluar las necesidades del proyecto y las habilidades del equipo de desarrollo para elegir la base de datos adecuada."))))))))
       else 
       (assert (herramienta "Si no estás buscando recomendaciones de bases de datos, ¿en qué puedo ayudarte?"))) 
       
       )

    


