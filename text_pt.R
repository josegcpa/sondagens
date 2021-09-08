website_legends  <- list(
  title = "São sondagens, senhor",
  plot_disc = "Partidos ao longo do tempo (discriminado)",
  plot_comp = "Partidos ao longo do tempo (completo)",
  plot_bars = "Comparação entre partidos por sondagem",
  plot_vars = "Variação dos valores ao longo do tempo",
  code = "Código",
  contact = "Contacto",
  log_scale = "Escala logarítmica",
  header_1 = "Ferramenta de análise de sondagens",
  add_data_csv = "Acrescentar dados (csv)",
  add_data_manual = "Acrescentar dados (manual)",
  streamgraph = "Gráfico de fluxo",
  calculate_error = "Calcular a margem de erro",
  compare_proportions = "Comparar proporções")

website_explanations <- list()

website_explanations$add_data_csv_explanation <- p(
  "Ficheiro separado por vírgulas com o seguinte formato:",
  "Partido, Total de sondados, Data, Proporção, Identificador da Sondagem",
  br(),
  "(a primeira linha não é utilizada)")

website_explanations$tool_explanation_text <- p(
  "O intuito desta pequena ferramenta é criar uma maneira simples de verificar se",
  "variações em diferentes sondagens são estatisticamente significantes ou se isso",
  "se trata apenas de ruído. As sondagens carregadas por defeito são descarregadas",
  "directamente do website",a("Europe Elects",href = 'https://europeelects.eu/'),
  "cada vez que esta plataforma é carregada."
)

website_explanations$kalman_explanation_text <- p(
  "Esta janela permite-nos apresentar uma aproximação dos resultados",
  "das sondagens ao longo do tempo recorrendo a um filtro de Kalman,",
  "que tipicamente é usado noutras abordagens semelhantes.",
  "Afeta os gráficos que observam os valores das diferentes sondagens",
  "ao longo do tempo.",
  "Como este processo pode demorar algum tempo, é melhor desativar",
  "este filtro enquanto estiverem a atualizar entradas na tabela.")

website_explanations$disclaimer_text <- p(
  helpText(
    "Esta plataforma não guarda os vossos dados. Se quiserem trabalhar com ela e continuar",
    "a trabalhar mais tarde por favor façam download dos dados em formato simplificado",
    "(em Download dos dados) e carreguem-nos para continuar (Acrescentar dados (csv))."
  )
)

website_explanations$help_text <- p(
  p(
    "Esta pequena ferramenta é útil para jornalistas/o público geral",
    "quando precisam/querem saber se uma variação numa sondagem é ruído",
    "ou sinal. Em termos práticos, esta pequena página permite-nos criar",
    "o nosso próprio conjunto de dados com o menu em baixo (ou carregar um",
    "conjunto de dados que tenhamos em csv) e visualizar essas mudanças.",
    "Graças a um teste estatístico simples - o teste exato de Fisher - conseguimos",
    "ver se uma variação na sondagem é ou não ruído - se um valor for",
    "estatisticamente significante é porque há de facto uma variação a ser reportada",
    "(aumento ou diminuição). Caso contrário podemos estar a reportar algo que não passa",
    "de ruído estatístico."
  )
)

website_explanations$statistical_significance_text <- p(
  p(
    "Uma análise, por norma, baseia-se numa amostra da população",
    "(uma sondagem é um exemplo disto, baseandos-se num determinado número de indivíduos",
    "para tirar conclusões sobre uma população - no caso das sondagens de intenção",
    "de voto em Portugal a população são os eleitores e a amostra são os indivíduos",
    "selecionados para responder a uma entrevista).",
    "Apesar de podermos tirar algumas conclusões quanto à nossa população, temos de",
    "ter cuidado para não tirar conclusões que não têm significado de um ponto de",
    "vista estatístico e não devem, portanto, ser consideradas enquanto conclusões",
    "válidas."
  ),
  p(
    "A", strong("significância estatística"), "é, no fundo esta validade em termos estatísticos",
    "e permite-nos verificar se as nossas conclusões - mudanças na proporção de",
    "pessoas que pretendem votar num partido ou noutro - são válidas (sinal) ou",
    "inválidas (ruído)."
  )
)

website_explanations$intervals_text <- p(
  p(
    "As proporções calculadas a partir de amostras têm um outro problema - são estimativas",
    "e, como qualquer estimativa, são imprecisas e têm associadas a elas um intervalo de",
    "valores possíveis associados a elas. Este intervalo é comummente designado de",
    strong("intervalo de confiança"),"e tem a ele associado um valor de confiança.",
    "Este valor de confiança, por sua vez, refere-se à probabilidade do nosso valor estar",
    "contido nesse intervalo - por exemplo, um intervalo de confiança de 95% que fica entre",
    "0.30 e 0.502 diz-nos que temos 95% de certezas que o nosso valor está contido entre",
    "0.30 e 0.502. Ao aumentarmos o valor de confiança de 95% para 99% podemos esperar",
    "que o nosso intervalo também aumente."),
  p(
    "Como tal, concluir que partido X duplicou a sua intenção de voto a partir de uma",
    "sondagem ou que outro teve uma variação de 5% é giro, mas incorrecto.",
    "O que podemos dizer é que houve um aumento/dimuição entre 2% e 5%, por exemplo.",
    "A última janela (Variações dos valores ao longo do tempo) pretende comunicar precisamente",
    "isto. Por norma, se o intervalo para a variação contem o valor 0 é porque não há um",
    "aumento sobre o qual valha a pena falar."
  )
)

website_explanations$calculate_error <- p(
  "Calcula o erro para um dado tamanho de amostra e e uma dada proporção."
)
