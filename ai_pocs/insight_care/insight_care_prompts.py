from enum import Enum


class Continue(Enum):
    TRUE = "TRUE"
    FALSE = "FALSE"
    UNDETERMINED = "UNDETERMINED"


class Prompts(Enum):
    SYSTEM_PROMPT = "Quiero que a partir de ahora actúes como un sicólogo, no puedes dar guías ni tratamientos en ninguna respuesta, si dejar sugerencias. Quiero que siempre te comuniques en ingles"
    MESSAGE_VARIATION_PROMPT = """Dado el siguiente mensaje delimitado por []:

    [{msg}]

    Genera una variación que respete el significado, no incluyas los [] en la respuesta"""
    CONFIRMATION_INQUIRE_CONTINUE_PROMPT = "Responde siguiendo el contexto de la conversación y tu objetivo es obtener una respuesta a la siguiente pregunta [{question}]"
    SHOULD_CONTINUE_PROMPT = "Determinar a partir del contexto anterior si el usuario respondió afirmativamente a la siguiente pregunta [{question}]. Quiero que me devuelvas únicamente una de las siguientes opciones " + Continue.TRUE.value + ", " + Continue.FALSE.value + " o " + Continue.UNDETERMINED.value
    WHO_RE_QUESTION_PROMPT = "Genera un mensaje que explique que no pudiste obtener una respuesta clara a la pregunta anterior y que necesitas que la respondan nuevamente basándote en las interacciones previas"
    WHO_ANALYSIS_PROMPT = "Determina a partir de la respuesta anterior una calificación del 1 al 6, retorna unicamente el entero con el valor, en caso de no detectar el valor retorna 0"
    WHO_INQUIRE = "Utiliza el siguiente listado de preguntas [{questions}] como referencia para indagar sobre la respuesta a la pregunta [{question}] basándote en el contexto anterior, trata de mantener una conversación coherente, concreta y breve buscando identificar cuales son los factores que contribuyen al estado de animo del usuario, todas los mensajes deben ser abiertos y sin preguntas con opciones de respuesta"
    WHO_SHOULD_INQUIRE = (
            "Determina si es necesario indagar más sobre la respuesta a la pregunta original teniendo en consideración que la pregunta original es [{question}] y el listado de preguntas para seguir indagando son [{questions}], trata de que no se extienda mucho el intercambio pero buscando identificar cuales son los factores que contribuyen al estado de animo del usuario. Como respuesta devuelve únicamente una de las siguientes opciones "
            + Continue.TRUE.value
            + " o "
            + Continue.FALSE.value
    )
    WHO_INQUIRE_CLOSE = "Dale un cierre a la interacción anterior agradeciendo o dando una conclusión dependiendo del contexto y agrega la siguiente pregunta en ingles sin modificarla y respetando su contenido {question}"
    SHOULD_KEEP_TALKING = "Determina basándote en el contexto anterior si el usuario desea seguir conversando sobre la pregunta {question} Como respuesta devuelve únicamente una de las siguientes opciones " + Continue.TRUE.value + " o " + Continue.FALSE.value
    KEEP_TALKING_PROMPT = "Continua la conversación basándote en el contexto anterior y tratando de concluir la pregunta {question}"
    ASSESSMENT_PROMPT = """Quiero que a partir del siguiente puntaje {score} y basándote en el contexto de la conversación y del siguiente ejemplo me generes un mensaje de evaluación y recomendaciones.
    
    El ejemplo es el siguiente:

    {example}

 """
    FINAL_REPORT_PROMPT = """Quiero que generes un informe final con todo lo discutido en la conversación, incluyendo: 
    
    * La información recolectada a partir de las preguntas
    * El puntaje final utilizado para la evaluación y recomendaciones
    * Las recomendaciones generadas a partir del puntaje

    Para generar este resumen evita incluir datos sensibles o personales del usuario, solo la información relevante para el informe final al final del informe realiza la pregunta de si esta de acuerdo en registrar el informe en su historia clínica
    """
    CLOSER_PROMPT = "Genera un mensaje de cierre para finalizar la conversación, incluyendo un agradecimiento por la participación y la disposición a seguir ayudando en el futuro y avisando a partir del mensaje anterior si se va a generar el reporte o no"
    TERMINATE_PROMPT = "Genera un mensaje de cierre para finalizar la conversación, incluyendo un agradecimiento por la participación, que lamentas no poder continuar con la charla y que estas a disposición para asistir en el futuro"


class Messages(Enum):
    INITIAL_MESSAGE = "Hola, soy tu asistente de bienestar. Me alegra que estés aquí. ¿Qué te parece si charlamos un poco sobre cómo te has estado sintiendo últimamente?"
    AGREEMENT_MESSAGE = "Genial. Antes de empezar, quiero que sepas que nuestra conversación es confidencial. También es importante que entiendas que no soy un médico y no puedo hacer diagnósticos ni dar tratamientos. Mi función es ayudarte a registrar tu nivel de bienestar además de ofrecerte algunas sugerencias generales. Si en algún momento sientes que necesitas ayuda profesional, no dudes en buscarla o solicitarmelo así podré ayudarte conectándote a un servicio especializado, ¿Estás de acuerdo en mantener esta conversación?"
    EXPLANATION_MESSAGE = "Me alegro de que lo entiendas. Ahora, me gustaría hacerte algunas preguntas sobre cómo te has sentido en las últimas dos semanas. Son preguntas específicas que nos ayudarán a tener una idea más clara de tu bienestar. Para cada pregunta, te daré seis opciones de respuesta. No hay respuestas correctas o incorrectas, solo quiero entender tu experiencia. ¿Te parece bien si empezamos?"

    WHO_1_MESSAGE = """La primera pregunta es: "¿Con qué frecuencia se ha sentido alegre y de buen humor?"

        Tus opciones son:
        1. Todo el tiempo
        2. La mayor parte del tiempo
        3. Más de la mitad del tiempo
        4. Menos de la mitad del tiempo
        5. De vez en cuando
        6. Nunca

        ¿Cuál de estas opciones describe mejor cómo te has sentido?
        """
    WHO_2_MESSAGE = """Ahora, pasemos a la siguiente pregunta: "¿Con qué frecuencia se ha sentido tranquilo y relajado?"

        Las opciones son las mismas:
        1. Todo el tiempo
        2. La mayor parte del tiempo
        3. Más de la mitad del tiempo
        4. Menos de la mitad del tiempo
        5. De vez en cuando
        6. Nunca

        """
    WHO_3_MESSAGE = """Ahora, continuemos con la tercera pregunta: “¿Con qué frecuencia se ha sentido activo y enérgico?"

        Las opciones siguen siendo:
        1. Todo el tiempo
        2. La mayor parte del tiempo
        3. Más de la mitad del tiempo
        4. Menos de la mitad del tiempo
        5. De vez en cuando
        6. Nunca

        ¿Cuál refleja mejor tu experiencia?
        """
    WHO_4_MESSAGE = """Ahora, pasemos a la cuarta pregunta: "¿Con qué frecuencia se ha sentido fresco y descansado?"

        Las opciones son:
        1. Todo el tiempo
        2. La mayor parte del tiempo
        3. Más de la mitad del tiempo
        4. Menos de la mitad del tiempo
        5. De vez en cuando
        6. Nunca

        ¿Cuál dirías que se aplica mejor a ti?
        """
    WHO_5_MESSAGE = """Ahora, vamos a la última pregunta"¿Con qué frecuencia su vida cotidiana ha estado llena de cosas que le interesan?"

        Y las opciones son:
        1. Todo el tiempo
        2. La mayor parte del tiempo
        3. Más de la mitad del tiempo
        4. Menos de la mitad del tiempo
        5. De vez en cuando
        6. Nunca

        ¿Cuál de estas opciones describe mejor tu experiencia?
        """
    QUESTIONS = {
        "WHO_1": {
            1: [
                "Es maravilloso escuchar que te has sentido tan bien. ¿Hay algo en particular que crees que ha contribuido a este estado de ánimo positivo?",
                "Me alegra mucho que hayas experimentado un estado de ánimo tan positivo. ¿Te gustaría compartir alguna experiencia que haya sido especialmente agradable?",
                "Es realmente alentador que te hayas sentido así. ¿Cómo crees que podrías mantener este buen humor en el futuro?",
            ],
            2: [
                "Es muy positivo que hayas tenido un buen estado de ánimo con tanta frecuencia. ¿Puedes identificar qué cosas te ayudan a mantener este buen humor?",
                "Me alegra escuchar que te has sentido bien la mayor parte del tiempo. ¿Hay algo que crees que podría ayudarte a sentirte así aún más a menudo?",
                "Es genial que hayas experimentado momentos de alegría con tanta frecuencia. ¿Qué actividades o situaciones suelen contribuir a tu buen humor?",
            ],
            3: [
                "Es positivo que hayas tenido momentos de alegría con cierta frecuencia. ¿Hay algo en particular que te ayude a mantener ese buen humor?",
                "Me alegra saber que has experimentado buen humor más de la mitad del tiempo. ¿Te gustaría compartir qué cosas te hacen sentir así?",
                "Es bueno que hayas tenido momentos de alegría regularmente. ¿Crees que hay algo que podrías hacer para aumentar estos momentos?",
            ],
            4: [
                "Entiendo que tu estado de ánimo ha tenido altibajos. ¿Hay algo en particular que crees que haya influido en cómo te has sentido?",
                "Gracias por compartir eso. A veces es difícil mantener un buen estado de ánimo. ¿Puedes identificar qué cosas te ayudan a sentirte mejor cuando estás decaído?",
                "Aprecio tu honestidad. ¿Te gustaría hablar sobre qué tipo de cosas podrían ayudarte a sentirte más alegre con mayor frecuencia?",
            ],
            5: [
                "Entiendo que no te has sentido alegre con mucha frecuencia. ¿Hay algo que esté pasando en tu vida que crees que está afectando tu estado de ánimo?",
                "Gracias por compartir eso. Incluso en momentos difíciles, es importante reconocer esos momentos de alegría. ¿Puedes recordar alguna situación reciente que te haya hecho sentir bien?",
                "Aprecio que me cuentes cómo te sientes. ¿Te gustaría explorar algunas ideas sobre cómo podrías aumentar esos momentos de buen humor?",
            ],
            6: [
                "Lamento escuchar que no has experimentado momentos de alegría últimamente. ¿Te gustaría hablar sobre lo que ha estado sucediendo en tu vida?",
                "Entiendo que este ha sido un período difícil para ti. ¿Hay algo que solía hacerte sentir mejor en el pasado que podríamos considerar intentar de nuevo?",
                "Agradezco mucho tu sinceridad al compartir esto. ¿Te parece si exploramos juntos algunas formas de mejorar tu bienestar emocional?",
            ],
        },
        "WHO_2": {
            1: [
                "Es maravilloso que hayas podido mantener un estado de calma constante. ¿Qué estrategias o actividades crees que te han ayudado a lograrlo?",
                "Me alegra mucho escuchar que te has sentido tan tranquilo. ¿Hay alguna práctica en particular que hayas encontrado especialmente útil para mantener esta calma?",
                "Es realmente positivo que hayas experimentado tanta tranquilidad. ¿Cómo crees que podrías compartir o aplicar esta experiencia en otras áreas de tu vida?",
            ],
            2: [
                "Es muy bueno saber que has podido sentirte tranquilo con tanta frecuencia. ¿Puedes identificar qué factores contribuyen a tu sensación de calma?",
                "Me alegra que hayas experimentado tanta tranquilidad. ¿Hay algo en particular que hagas cuando te sientes estresado para recuperar esa sensación de calma?",
                "Es genial que hayas logrado mantener la calma la mayor parte del tiempo. ¿Cómo crees que esta tranquilidad ha afectado otros aspectos de tu vida?",
            ],
            3: [
                "Es positivo que hayas podido sentirte tranquilo con cierta regularidad. ¿Hay alguna actividad o situación que notes que te ayuda a sentirte más relajado?",
                "Me alegra saber que has experimentado calma más de la mitad del tiempo. ¿Te gustaría compartir qué estrategias usas para mantener esa tranquilidad?",
                "Es bueno que hayas tenido momentos de calma regularmente. ¿Crees que hay algo que podrías hacer para aumentar estos momentos de tranquilidad?",
            ],
            4: [
                "Entiendo que has tenido dificultades para sentirte tranquilo últimamente. ¿Hay algo en particular que crees que está afectando tu capacidad para relajarte?",
                "Gracias por compartir eso. A veces puede ser difícil encontrar momentos de calma. ¿Puedes recordar alguna situación reciente en la que te hayas sentido más relajado?",
                "Aprecio tu honestidad. ¿Te gustaría que exploráramos juntos algunas técnicas que podrían ayudarte a sentirte más tranquilo?",
            ],
            5: [
                "Entiendo que no te has sentido tranquilo con mucha frecuencia. ¿Hay algo específico que sientes que está contribuyendo a tu inquietud o estrés?",
                "Gracias por compartir eso. Incluso en momentos de estrés, es importante reconocer esos momentos de calma. ¿Puedes identificar qué te ayuda a sentirte más relajado cuando ocurren?",
                "Aprecio que me cuentes cómo te sientes. ¿Te interesaría aprender algunas técnicas de relajación que podrías practicar en tu día a día?",
            ],
            6: [
                "Lamento escuchar que no has podido sentirte tranquilo últimamente. ¿Te gustaría hablar sobre lo que ha estado causando tanta tensión en tu vida?",
                "Entiendo que este ha sido un período muy estresante para ti. ¿Hay algo que solías hacer en el pasado que te ayudaba a relajarte y que podríamos considerar retomar?",
                "Agradezco mucho tu sinceridad al compartir esto. ¿Qué te parece si trabajamos juntos para encontrar formas de introducir más momentos de calma en tu vida?",
            ],
        },
        "WHO_3": {
            1: [
                "Es fantástico escuchar que te has sentido tan lleno de energía. ¿Qué actividades o hábitos crees que han contribuido a mantener este nivel de energía?",
                "Me alegra mucho que hayas experimentado tanta vitalidad. ¿Hay algo en particular que hayas estado haciendo diferente que crees que ha influido en tu nivel de energía?",
                "Es realmente positivo que te hayas sentido tan activo. ¿Cómo crees que este nivel de energía ha afectado otras áreas de tu vida?",
            ],
            2: [
                "Es muy bueno saber que te has sentido activo y enérgico con tanta frecuencia. ¿Puedes identificar qué factores contribuyen a mantener tu energía alta?",
                "Me alegra que hayas experimentado tanta vitalidad. ¿Hay alguna rutina o hábito en particular que hayas adoptado que crees que te ayuda a mantener tu energía?",
                "Es genial que hayas logrado mantener un buen nivel de energía la mayor parte del tiempo. ¿Cómo crees que esto ha impactado en tu vida diaria y en tus actividades?",
            ],
            3: [
                "Es positivo que hayas podido sentirte activo y enérgico con cierta regularidad. ¿Hay alguna actividad o situación que notes que te ayuda a mantener tu energía?",
                "Me alegra saber que has experimentado vitalidad más de la mitad del tiempo. ¿Te gustaría compartir qué cosas haces para mantener tu nivel de energía?",
                "Es bueno que hayas tenido momentos de actividad y energía regularmente. ¿Crees que hay algo que podrías hacer para aumentar aún más estos momentos de vitalidad?",
            ],
            4: [
                "Entiendo que has tenido dificultades para sentirte activo y enérgico últimamente. ¿Hay algo en particular que crees que está afectando tus niveles de energía?",
                "Gracias por compartir eso. A veces puede ser difícil mantener la energía alta. ¿Puedes recordar algún momento reciente en el que te hayas sentido más activo y qué lo provocó?",
                "Aprecio tu honestidad. ¿Te gustaría que exploráramos juntos algunas formas de aumentar tu nivel de energía en el día a día?",
            ],
            5: [
                "Entiendo que no te has sentido muy activo o enérgico con frecuencia. ¿Hay algo específico que sientes que está contribuyendo a tu falta de energía?",
                "Gracias por compartir eso. Incluso en momentos de baja energía, es importante reconocer esos momentos en los que te sientes más activo. ¿Puedes identificar qué te ayuda a sentirte más enérgico cuando ocurre?",
                "Aprecio que me cuentes cómo te sientes. ¿Te interesaría explorar algunas estrategias para aumentar tu nivel de energía de manera gradual?",
            ],
            6: [
                "Lamento escuchar que no te has sentido activo o enérgico últimamente. ¿Te gustaría hablar sobre lo que ha estado afectando tus niveles de energía?",
                "Entiendo que este ha sido un período de baja energía para ti. ¿Hay algo que solías hacer en el pasado que te ayudaba a sentirte más activo y que podríamos considerar retomar?",
                "Agradezco mucho tu sinceridad al compartir esto. ¿Qué te parece si trabajamos juntos para encontrar formas de aumentar gradualmente tu nivel de actividad y energía?",
            ],
        },
        "WHO_4": {
            1: [
                "Es maravilloso escuchar que te has sentido tan descansado. ¿Qué hábitos o rutinas crees que han contribuido a tu buena calidad de sueño?",
                "Me alegra mucho que hayas experimentado un descanso tan consistente. ¿Hay algo en particular en tu rutina nocturna que crees que ha sido especialmente beneficioso?",
                "Es realmente positivo que te hayas sentido fresco y descansado constantemente. ¿Cómo crees que este buen descanso ha afectado otros aspectos de tu vida?",
            ],
            2: [
                "Es muy bueno saber que te has sentido descansado con tanta frecuencia. ¿Puedes identificar qué factores contribuyen a tu buen descanso?",
                "Me alegra que hayas experimentado un buen descanso la mayor parte del tiempo. ¿Hay alguna estrategia en particular que uses para asegurar un buen sueño?",
                "Es genial que hayas logrado sentirte fresco y descansado la mayor parte del tiempo. ¿Cómo crees que esto ha impactado en tu energía y estado de ánimo durante el día?",
            ],
            3: [
                "Es positivo que hayas podido sentirte descansado con cierta regularidad. ¿Hay algo en particular que notes que te ayuda a tener un mejor descanso?",
                "Me alegra saber que te has sentido fresco más de la mitad del tiempo. ¿Te gustaría compartir qué haces en las noches que logras descansar bien?",
                "Es bueno que hayas tenido un buen descanso regularmente. ¿Crees que hay algo que podrías hacer para mejorar aún más la calidad de tu sueño?",
            ],
            4: [
                "Entiendo que has tenido dificultades para sentirte descansado últimamente. ¿Hay algo en particular que crees que está afectando tu sueño?",
                "Gracias por compartir eso. A veces puede ser difícil lograr un buen descanso. ¿Puedes recordar alguna noche reciente en la que hayas dormido mejor y qué fue diferente?",
                "Aprecio tu honestidad. ¿Te gustaría que exploráramos juntos algunas técnicas que podrían ayudarte a mejorar la calidad de tu sueño?",
            ],
            5: [
                "Entiendo que no te has sentido fresco y descansado con mucha frecuencia. ¿Hay algo específico que sientes que está interfiriendo con tu descanso?",
                "Gracias por compartir eso. Incluso en momentos menos interesantes, es importante reconocer esas noches en las que duermes bien. ¿Puedes identificar qué fue diferente en esos momentos?",
                "Aprecio que me cuentes cómo te sientes. ¿Te interesaría explorar algunas estrategias para mejorar la calidad de tu sueño?",
            ],
            6: [
                "Lamento escuchar que no te has sentido descansado últimamente. ¿Te gustaría hablar sobre lo que ha estado afectando tu sueño?",
                "Entiendo que este ha sido un período difícil para descansar. ¿Hay algo que solías hacer en el pasado que te ayudaba a dormir mejor y que podríamos considerar retomar?",
                "Agradezco mucho tu sinceridad al compartir esto. ¿Qué te parece si trabajamos juntos para encontrar formas de mejorar gradualmente la calidad de tu descanso?",
            ],
        },
        "WHO_5": {
            1: [
                "Es fantástico escuchar que tu vida está llena de cosas que te interesan. ¿Podrías compartir algunas de las actividades o intereses que te mantienen tan involucrado?",
                "Me alegra mucho que encuentres tanto interés en tu vida diaria. ¿Cómo crees que esto ha influido en tu bienestar general?",
                "Es realmente positivo que te sientas tan interesado en tus actividades diarias. ¿Hay algún nuevo interés o pasatiempo que hayas descubierto recientemente?",
            ],
            2: [
                "Es muy bueno saber que encuentras interés en tu vida cotidiana con tanta frecuencia. ¿Puedes compartir algunas de las cosas que más te interesan?",
                "Me alegra que tu vida esté llena de cosas interesantes la mayor parte del tiempo. ¿Cómo logras mantener ese nivel de interés en tus actividades diarias?",
                "Es genial que hayas encontrado tanto interés en tu vida. ¿Hay alguna área en particular de tu vida que sientas que es especialmente satisfactoria?",
            ],
            3: [
                "Es positivo que tu vida esté llena de cosas interesantes con cierta regularidad. ¿Hay algunas actividades en particular que disfrutes más?",
                "Me alegra saber que encuentras interés en tu vida más de la mitad del tiempo. ¿Te gustaría compartir qué tipo de cosas te mantienen más involucrado?",
                "Es bueno que hayas encontrado interés en tu vida cotidiana regularmente. ¿Crees que hay algo que podrías hacer para aumentar aún más estos momentos de interés?",
            ],
            4: [
                "Entiendo que has tenido dificultades para encontrar cosas interesantes en tu vida últimamente. ¿Hay algo en particular que crees que está afectando tu nivel de interés?",
                "Gracias por compartir eso. A veces puede ser difícil mantenerse interesado. ¿Puedes recordar alguna actividad reciente que hayas disfrutado particularmente?",
                "Aprecio tu honestidad. ¿Te gustaría que exploráramos juntos algunas formas de introducir más actividades interesantes en tu vida diaria?",
            ],
            5: [
                "Entiendo que no encuentras muchas cosas interesantes en tu vida cotidiana con frecuencia. ¿Hay algo específico que sientes que está contribuyendo a esta falta de interés?",
                "Gracias por compartir eso. Incluso en momentos menos interesantes, es importante reconocer esas ocasiones en las que sí encuentras algo que te motiva. ¿Puedes identificar qué tipo de cosas te resultan más atractivas?",
                "Aprecio que me cuentes cómo te sientes. ¿Te interesaría explorar algunas ideas para descubrir nuevos intereses o retomar antiguos pasatiempos?",
            ],
            6: [
                "Lamento escuchar que no has encontrado cosas interesantes en tu vida últimamente. ¿Te gustaría hablar sobre lo que ha estado afectando tu nivel de interés?",
                "Entiendo que este ha sido un período difícil para encontrar motivación. ¿Hay alguna actividad o interés que solías disfrutar en el pasado y que te gustaría retomar?",
                "Agradezco mucho tu sinceridad al compartir esto. ¿Qué te parece si trabajamos juntos para explorar nuevas actividades o intereses que puedan añadir más satisfacción a tu vida diaria?",
            ],
        },
    }

    ASSESSMENTS_MESSAGES = [
        {
            "score_start": 0,
            "score_end": 28,
            "example": """
                    
                Me preocupa lo que me has contado. Parece que las cosas han estado bastante difíciles últimamente. Basándome en lo que compartiste, tengo algunas sugerencias que podrían ayudarte:

                * Buscar ayuda profesional: Sería importante que contactes a un médico o psicólogo lo antes posible. Ellos tienen las herramientas para ayudarte en esta situación.
                * Recursos de emergencia: Existen líneas de ayuda disponibles 24/7 que podrían ser útiles en momentos de crisis.
                * Apoyo social: Trata de no quedarte solo. Considera contactar a un familiar o amigo de confianza.
                * Técnicas de manejo del estrés: Hay recursos en línea y aplicaciones que ofrecen técnicas básicas de relajación.

                ¿Alguna de estas sugerencias te parece útil?""",
        },
        {
            "score_start": 29,
            "score_end": 50,
            "example": """
                    
                Gracias por compartir cómo te has estado sintiendo. Parece que has tenido algunos desafíos últimamente. Tengo algunas sugerencias que podrían ayudarte:

                * Evaluación profesional: Considera consultar con un profesional de salud mental para una evaluación más detallada.
                * Hábitos saludables: Intenta mejorar tu rutina de sueño, alimentación y ejercicio.
                * Apoyo social: Busca oportunidades para conectar con personas de confianza.
                * Actividades de bienestar: Explora recursos sobre meditación o yoga disponibles en línea o en tu comunidad.

                ¿Alguna de estas ideas te resulta interesante?""",
        },
        {
            "score_start": 51,
            "score_end": 70,
            "example": """
            
                Aprecio que hayas compartido cómo te sientes. Parece que hay algunas cosas que te están pesando un poco. Aquí hay algunas sugerencias que podrían ayudarte a mejorar tu bienestar:

                * Autocuidado: Presta atención a las señales de estrés y busca formas de cuidarte mejor.
                * Técnicas de relajación: ¿Has probado el mindfulness? Podríamos explorar algunas técnicas simples.
                * Rutina saludable: Hablemos sobre tu rutina de sueño, alimentación y ejercicio. Pequeños cambios pueden hacer una gran diferencia.
                * Actividades satisfactorias: ¿Qué hobbies o actividades sociales te gustaría retomar o probar?
                
                ¿Cuál de estas áreas te gustaría explorar primero?""",
        },
        {
            "score_start": 71,
            "score_end": 100,
            "example": """
                    
                Me alegra escuchar que en general te has estado sintiendo bien. ¡Eso es genial! Aquí hay algunas ideas para mantener y potenciar aún más tu bienestar:

                * Mantener lo positivo: ¿Qué hábitos crees que están contribuyendo más a tu bienestar actual?
                * Compartir estrategias: Tu experiencia podría ser valiosa para otros. ¿Has pensado en cómo podrías compartir tus estrategias de bienestar?
                * Nuevas metas: Este puede ser un buen momento para establecer nuevos objetivos. ¿Hay algo que siempre hayas querido intentar?
                * Crecimiento personal: ¿Qué áreas de tu vida te gustaría desarrollar aún más?
        
                ¿Alguna de estas ideas te emociona particularmente?""",
        },
    ]
