library(shiny)
library(bslib)
library(ggplot2)
library(shinyBS)
library(shinyjs)

pop_test <- c("Mean Test", "Proportion Test", "Variance Test")
sd_Type <- c("Population", "Sample")
sd_Type <- c("Population", "Sample")
param_assumption <- c("Equals", "Not Equals")
properTest <- c("One-Tailed Left", "One-Tailed Right", "Two-Tailed")

######################################################################################################
# UI MODULE

render_homeTab_ui <- function(id, type){
  ns <- NS(id)
  
  tagList(
    sidebarPanel(class="sidebarPanel", width=2,
      uiOutput("sidebarInput_ui"),
      numericInput("conf_int", "Confidence Level:", value = 0.95, min = 0, max = 1),
      selectInput("properTest", label = "Alternative:", choices = properTest, selected = "Two-Tailed"),
      if(type == "one"){
        actionButton(class="calculate-btn", inputId =  "calculate1", label = "Calculate!")
      } else if(type=="two") {
        actionButton(class="calculate-btn", inputId = "calculate2", label = "Calculate!")
      }
    ),
    mainPanel(class="mainPanel", width=8,
      p("Hasil Pengujian Hipotesis", style="font-weight: bold; color:#10428D; font-size: 25px; margin-bottom: 20px;"),
      
      div(
        class = "card-parent",
        div(class = "card-header-parent", "Distribution Plot"),
        div(class = "card-body-parent",
            if(type == "one"){
              uiOutput("dynamic_plot1")  
            } else if(type=="two"){
              uiOutput("dynamic_plot2")
            }
        )
      ),
      
      div(
        class = "card-parent",
        div(class = "card-header-parent", "Step-by-Step Guide"),
        div(class = "card-body-parent",
            if(type == "one"){
              uiOutput("step_result1")
            } else if (type == "two"){
              uiOutput("step_result2")
            }
        )
      )
    )
  )
}

render_aboutInfo_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="welcoming-title", "About This Website"),
    
    div(class = "Welcoming-container",
        div(class = "welcome", 
            "Selamat Datang di Website Hypothesis Testing!"
        ),
        div(class = "welcom-guide",
            "Website ini dirancang untuk membantu Anda melakukan uji hipotesis satu dan dua populasi. Pastikan data yang Anda gunakan memenuhi asumsi distribusi normal agar hasil pengujian dapat valid. Untuk memulai, cukup tekan tombol 'One Population Test' atau 'Two Population Test' di bawah ini dan ikuti langkah-langkahnya."
        )
    ),
    
    div(class="welcoming-title", "Our Developer"),
    
    div(class="container-card", style="margin-top:20px",
        div(class="row-card",
            div(class="col-card",
                div(
                  id = "profile_card",
                  class = "profile-card",
                  div(
                    class = "profile-card-header",
                    img(
                      src = "Tampak_dada.jpg",
                      alt = "Profile Picture",
                      class = "profile-picture"
                    )
                  ),
                  div(
                    class = "profile-card-body",
                    h2(class = "name", "John Doe"),
                    p(class = "role", "Software Engineer"),
                    p(
                      class = "bio",
                      "Passionate about building scalable web applications and exploring AI. Loves hiking and photography during weekends."
                    ),
                    div(
                      class = "social-links",
                      a(href = "https://linkedin.com", "LinkedIn", target = "_blank"),
                      a(href = "https://github.com", "GitHub", target = "_blank"),
                      a(href = "https://twitter.com", "Twitter", target = "_blank")
                    )
                  )
                )
            ),
            div(class="col-card",
                
                div(
                  id = "profile_card",
                  class = "profile-card",
                  div(
                    class = "profile-card-header",
                    img(
                      src = "Tampak_dada.jpg",
                      alt = "Profile Picture",
                      class = "profile-picture"
                    )
                  ),
                  div(
                    class = "profile-card-body",
                    h2(class = "name", "John Doe"),
                    p(class = "role", "Software Engineer"),
                    p(
                      class = "bio",
                      "Passionate about building scalable web applications and exploring AI. Loves hiking and photography during weekends."
                    ),
                    div(
                      class = "social-links",
                      a(href = "https://linkedin.com", "LinkedIn", target = "_blank"),
                      a(href = "https://github.com", "GitHub", target = "_blank"),
                      a(href = "https://twitter.com", "Twitter", target = "_blank")
                    )
                  )
                )
            ),
            div(class="col-card",
                div(
                  id = "profile_card",
                  class = "profile-card",
                  div(
                    class = "profile-card-header",
                    img(
                      src = "Tampak_dada.jpg",
                      alt = "Profile Picture",
                      class = "profile-picture"
                    )
                  ),
                  div(
                    class = "profile-card-body",
                    h2(class = "name", "John Doe"),
                    p(class = "role", "Software Engineer"),
                    p(
                      class = "bio",
                      "Passionate about building scalable web applications and exploring AI. Loves hiking and photography during weekends."
                    ),
                    div(
                      class = "social-links",
                      a(href = "https://linkedin.com", "LinkedIn", target = "_blank"),
                      a(href = "https://github.com", "GitHub", target = "_blank"),
                      a(href = "https://twitter.com", "Twitter", target = "_blank")
                    )
                  )
                ),
            ),
            div(class="col-card",
                div(
                  id = "profile_card",
                  class = "profile-card",
                  div(
                    class = "profile-card-header",
                    img(
                      src = "Tampak_dada.jpg",
                      alt = "Profile Picture",
                      class = "profile-picture"
                    )
                  ),
                  div(
                    class = "profile-card-body",
                    h2(class = "name", "John Doe"),
                    p(class = "role", "Software Engineer"),
                    p(
                      class = "bio",
                      "Passionate about building scalable web applications and exploring AI. Loves hiking and photography during weekends."
                    ),
                    div(
                      class = "social-links",
                      a(href = "https://linkedin.com", "LinkedIn", target = "_blank"),
                      a(href = "https://github.com", "GitHub", target = "_blank"),
                      a(href = "https://twitter.com", "Twitter", target = "_blank")
                    )
                  )
                ),
            )
        ),
    ),
    
    sidebarLayout(
      uiOutput(NULL),
      mainPanel(
        NULL
      )
    )
    
  )
}

render_homeNav_ui <- function(id){
  tagList(
    div(id = "analysis_navigation", class="home_nav_row", style="display: flex; justify-content: space-between; align-items: center;",
        actionButton(class="home_nav", style="padding-top:20px; padding-bottom: 20px;", inputId = "one_pop_panel", div(icon("user", class = "fa-solid fa-user"), span("One Population Test"))),
        actionButton(class="home_nav", style="padding-top:20px; padding-bottom: 20px;", inputId = "two_pop_panel", div(icon("user-group"), span("Two Population Test"))),
    ),
  )
}

sidebarInput_ui <- function(id, type){
  ns <- NS(id)
  
  if(type == "One Population"){
    tagList(
      selectInput("one_pop_test", "Select Test", choices = pop_test, selected = "Mean Test"),
      
      tabsetPanel(
        id = "popTest1",
        type = "hidden",
        tabPanel("Mean Test",
                 span("Mean: ", style="font-weight: bold"),
                 fluidRow(
                   column(6, numericInput("mu", "Population", value = 400)),
                   column(6, numericInput("mean", "Sample", value = 400))
                 ),
                 selectInput("sd_Type1", "Standard Deviation Type:", choices = sd_Type, selected = "Population"),
                 numericInput("sd", "Standard Deviation:", value = 35, min = 0.01),
                 numericInput("n_mean", "Sample Size:", value = 40, min = 1),
        ),
        tabPanel("Proportion Test",
                 span("Proportion: ", style="font-weight: bold"),
                 fluidRow(
                   column(6, numericInput("p_pop", "Population", value = 0.08)),
                   column(6, numericInput("p_sample", "Sample ", value = 0.05))
                 ),
                 numericInput("n_prop", "Sample Size:", value = 40, min = 1),
        ),
        tabPanel("Variance Test",
                 span("Variance: ", style="font-weight: bold"),
                 fluidRow(
                   column(6, numericInput("var_pop", "Population", value = 0.8)),
                   column(6, numericInput("var_sample", "Sample", value = 1.2))
                 ),
                 numericInput("n_var", "Sample Size:", value = 40, min = 1),
        )
      )
    )
  } else if(type == "Two Population"){
    tagList(
      selectInput("two_pop_test", "Select Test", choices = pop_test, selected = "Mean Test"),
      tabsetPanel(
        id = "popTest2",
        type = "hidden",
        tabPanel("Mean Test",
                 numericInput("mu_dif", "Population Mean Difference:", value = 88),
                 span("Sample: ", style="font-weight: bold"),
                 fluidRow(
                   column(6,
                          numericInput("mean1", "Mean 1", value = 85),
                   ),
                   column(6,
                          numericInput("mean2", "Mean 2", value = 81),  
                   )
                 ),
                 
                 selectInput("sd_Type2", "Standard Deviation Type:", sd_Type),
                 
                 span("Standard Deviation: ", style="font-weight: bold"),
                 fluidRow(
                   column(6,
                          numericInput("sd1", "SD 1", value = 4, min = 0.01)
                   ),
                   column(6,
                          numericInput("sd2", "SD 2", value = 5, min = 0.01)
                   )
                 ),
                 
                 uiOutput("sd_assumption"),
                 
                 span("Size: ", style="font-weight: bold;"),
                 fluidRow(
                   column(6,
                          numericInput("n_mean1", "Sample 1", value = 12, min = 1),
                   ),
                   column(6,
                          numericInput("n_mean2", "Sample 2", value = 10, min = 1)
                   )
                 ),
                 
        ),
        tabPanel("Proportion Test",
                 selectInput("prop_assumption", "Population Proportion Assumption", choices = param_assumption),
                 uiOutput("prop_params"),
                 
                 span("Target Sample:", style="font-weight: bold;"),
                 fluidRow(
                   column(6,
                          numericInput("n_interest1", "Size 1", value = 20),
                   ),
                   column(6,
                          numericInput("n_interest2", "Size 2", value = 25),
                   )
                 ),
                 
                 span("Total Size:", style="font-weight: bold;"),
                 fluidRow(
                   column(6,
                          numericInput("n_prop1", "Sample 1", value = 40, min = 1),
                   ),
                   column(6,
                          numericInput("n_prop2", "Sample 2", value = 40, min = 1)
                   )
                 ),
        ),
        tabPanel("Variance Test",
                 span("Variance:", style="font-weight: bold;"),
                 fluidRow(
                   column(6,
                          numericInput("var_sample1", "Sample 1", value = 1.2),
                   ),
                   column(6,
                          numericInput("var_sample2", "Sample 2", value = 1.5),
                   )
                 ),
                 
                 span("Size:", style="font-weight: bold;"),
                 fluidRow(
                   column(6,
                          numericInput("n_var1", "Sample 1", value = 40, min = 1),
                   ),
                   column(6,
                          numericInput("n_var2", "Sample 2", value = 40, min = 1),
                   )
                 ),
        )
      ) 
    )
  }
}

distributionPlot_ui <- function(id) {
  ns <- NS(id)  # Namespace for module
  tagList(
    plotOutput(ns("distributionPlot"))
  )
}


stepResult_ui <- function(id) {
  ns <- NS(id)  # Namespace for module
  tagList(
    uiOutput(ns("stepResult"))
  )
}

renderStepResult <- function(type, properTest, mu, conf_int, testStatistics, lower_cv, upper_cv, mean, n_sample = NULL, sd, testType, testVariables, p_pop, p_sample, var_pop, var_sample, mu_dif, mean1, mean2, sd1, sd2, n_sample1, n_sample2, sd_Type, sd_assumption, df, sd_pulled, p_diff, p_pulled, p_sample1, p_sample2, n_interest1, n_interest2, assumption, var_sample1, var_sample2, df1, df2){
  alpha <- (1-conf_int)
  
  ## p-value
  if(properTest == "Two-Tailed"){
      if(testType == "one_mean" | testType == "two_mean") {
        if(testVariables == "normal_test"){
          p_value <- 2 * round(pnorm(-abs(testStatistics), mean = 0, sd=1), 4)
        } else if(testVariables == "t_test") {
          if(testType == "two_mean"){
            p_value <- 2 * round(pt(-abs(testStatistics), df = df), 4)
          } else {
            p_value <- 2 * round(pt(-abs(testStatistics), df = n_sample-1), 4)
          }
        }
      } else if(testType == "one_prop" | testType == "two_prop") {
        p_value <- 2 * round(pnorm(testStatistics, mean = 0, sd=1), 4)
      } else if(testType == "one_var"){
        p_value <- 2 * round(min(pchisq(testStatistics, df = n_sample - 1), 1 - pchisq(testStatistics, df = n_sample - 1)), 4)
      } else if (testType == "two_var"){
        p_value <- 2 * round(min(pf(testStatistics, df1, df2), 1 - pf(testStatistics, df1, df2)), 4)
      }
    
  } else if (properTest == "One-Tailed Right"){
    if(testType == "one_mean" | testType == "two_mean"){
        if(testVariables == "normal_test"){
          p_value <- round(pnorm(testStatistics, mean = 0, sd=1, lower.tail = F), 4)
        } else if(testVariables == "t_test") {
          if(testType == "two_mean"){
            p_value <- round(pt(testStatistics, df = df, lower.tail = F), 4)
          } else {
            p_value <- round(pt(testStatistics, df = n_sample - 1, lower.tail = F), 4)
          }
          
        }
      } else if(testType == "one_prop" | testType == "two_prop"){
        p_value <- round(pnorm(testStatistics, mean = 0, sd=1, lower.tail = F), 4)
      } else if(testType == "one_var"){
        p_value <- round(1 - pchisq(testStatistics, df = n_sample - 1), 4)
      } else if(testType == "two_var") {
        p_value <- round(1 - pf(testStatistics, df1, df2), 4)
      }
  } else {
      if(testType == "one_mean" | testType == "two_mean"){
        if(testVariables == "normal_test"){
          p_value <- round(pnorm(testStatistics, mean = 0, sd=1), 4)
        } else if(testVariables == "t_test") {
          if(testType == "two_mean"){
            p_value <- round(pt(testStatistics, df = df), 4)
          } else {
            p_value <- round(pt(testStatistics, df = n_sample - 1), 4)
          }
          
        }
      }else if(testType == "one_prop" | testType == "two_prop"){
        p_value <- round(pnorm(testStatistics, mean = 0, sd=1), 4)
      } else if(testType == "one_var"){
        p_value <- round(pchisq(testStatistics, df = n_sample - 1), 4)
      } else if(testType == "two_var") {
        p_value <- round(pf(testStatistics, df1, df2), 4)
      }
  }

  switch (testType,
    "one_mean" = {
      parameter <- "\\mu"
      statistik <- "\\bar{x}"
      nilaiParam <- mu
      if(testVariables == "normal_test"){
        testSymbol <- "Z"
        sdSymbol <- "\\sigma"
      } else {
        testSymbol <- "t"
        sdSymbol <- "s"
      }
    },
    "one_prop" = {
      nilaiParam <- p_pop
      parameter <- "p_0"
      statistik <- "\\hat{p}"
      testSymbol <- "Z"
    },
    "one_var" = {
      nilaiParam <- var_pop
      parameter <- "\\sigma^{2}"
      statistik <- "\\s^{2}"
      testSymbol <- "\\chi^2"
    },
    "two_mean" = {
      parameter <- "\\mu_1 - \\mu_2"
      statistik <- "\\bar{x}_1 - \\bar{x}_2"
      nilaiParam <- mu_dif
      if(testVariables == "normal_test"){
        testSymbol <- "Z"
        sdSymbol1 <- "\\sigma_1^2"
        sdSymbol2 <- "\\sigma_2^2"
      } else if(testVariables == "t_test"){
        testSymbol <- "t"
        sdSymbol1 <- "s_1^2"
        sdSymbol2 <- "s_2^2"
      }
    },
    "two_prop" = {
      parameter <- "p_1 - p_2"
      statistik <- "\\hat{p}_1 - \\hat{p}_2"
      nilaiParam <- p_diff
      testSymbol <- "Z"
    }, 
    "two_var" = {
      parameter <- "\\sigma^{2}"
      testSymbol <- "f"
    },
  )
  
  if (properTest == "Two-Tailed"){
    simbol_hipotesis_0 <- "="
    simbol_hipotesis_1 <- "\\neq"
    alpha <- (1-conf_int)/2
    
    if(testStatistics > lower_cv && testStatistics < upper_cv){
      keputusan <- "Gagal Tolak \\(H_0 \\)"
      daerah <- "Terima"
    } else {
      keputusan <- "Tolak \\(H_0 \\)"
      daerah <- "Tolak"
    }
    
  } else if(properTest == "One-Tailed Left"){
    simbol_hipotesis_0 <- "\\leq"
    simbol_hipotesis_1 <- ">"
    
    if(testStatistics > lower_cv){
      keputusan <- "Gagal Tolak \\(H_0 \\)"
      daerah <- "Terima"
    } else {
      keputusan <- "Tolak \\(H_0 \\)"
      daerah <- "Tolak"
    }
    
  } else {
    simbol_hipotesis_0 <- "\\geq"
    simbol_hipotesis_1 <- "<"
    
    if(testStatistics < upper_cv){
      keputusan <- "Gagal Tolak \\(H_0 \\)"
      daerah <- "Terima"
    } else {
      keputusan <- "Tolak \\(H_0 \\)"
      daerah <- "Tolak"
    }
  }
  
  tagList(
    div(
      class = "step-result-card",
      div(class = "card-header-child", "Step 1: Menyatakan Hipotesis"),
      div(class = "card-body-child",
        p(paste("Karena pada tab Alternatif dipilih Uji", properTest, ", maka hipotesisnya adalah sebagai berikut.")),
        
        div(style= "text-align: left;",
            if(testType == "two_var"){
              tagList(
                p("\\( H_0 : \\sigma_1^2", simbol_hipotesis_0," \\sigma_2^2\\)"),
                p("\\( H_1 : \\sigma_1^2", simbol_hipotesis_1," \\sigma_2^2\\)"),
              )
            } else {
              tagList(
                p("\\( H_0 : ", parameter, simbol_hipotesis_0, nilaiParam, " \\)"),
                p("\\( H_1 : ", parameter, simbol_hipotesis_1, nilaiParam, " \\)")
              )
            }
        )
      )
    ),
    
    div(
      class = "step-result-card",
      div(class = "card-header-child", withMathJax("Step 2: Menentukan \\(\\alpha \\)")),
      div(class = "card-body-child",
        div(style= "text-align: left;",
          p("Pada tab \\(Confidence Level\\), nilai yang di Input adalah ", conf_int,
            ". Maka nilai \\(\\alpha\\) dapat dicari dengan:"
          ),
          p("\\(\\alpha: \\) 1 - ", conf_int, "=", 1-conf_int)
        )
      )
    ),
    
    div(
      class = "step-result-card",
      div(class = "card-header-child", withMathJax("Step 3: Menentukan Titik Kritis (Daerah Tolak)")),
      div(class = "card-body-child",
          div(style= "text-align: left;",
              p("Sebab memilih Uji ", properTest, ", maka titik kritisnya adalah:"),
              p("\\(\\frac{\\alpha}{2} \\) = ", alpha),
              if(properTest == "Two-Tailed"){
                tagList(
                  div(
                    p("\\(", testSymbol,"_{\\alpha} \\) = ", lower_cv),
                    p("\\(", testSymbol,"_{1-\\alpha} \\) = ", upper_cv),
                    p("Wilayah Kritis: daerah yang < ", lower_cv, " atau daerah yang > ", upper_cv)
                  )
                )
                
              } else {
                  if(properTest == "One-Tailed Left"){
                    tagList(
                      p("\\(", testSymbol,"_{1-\\alpha} \\) = ", lower_cv),
                      p("Wilayah Kritis: daerah yang < ", lower_cv),
                    )} else {
                    tagList(
                      p("\\(", testSymbol,"_{\\alpha} \\) = ", lower_cv),
                      p("Wilayah Kritis: daerah yang < ", lower_cv),
                    )
                  }
            }
              
          )
      )
    ),
    
    div(
      class = "step-result-card",
      div(class = "card-header-child", withMathJax("Step 4: Menghitung Statistik Uji atau Test Statistics (TS)")),
      div(class = "card-body-child",
          div(style= "text-align: left;",
            if(testType == "one_mean"){p("\\(", testSymbol,"_{hitung} = \\frac{",statistik," - \\mu_0}{\\frac{",sdSymbol,"}{\\sqrt{n}}} = \\frac{",mean,"-", mu, "}{\\frac{",sd,"}{\\sqrt{",n_sample,"}}} = ", round(testStatistics, 2),"\\)")}
            else if(testType == "one_prop"){p("\\(", testSymbol,"_{hitung} = \\frac{",statistik," - ", parameter,"}{\\sqrt{\\frac{p_{0}(1-p_{0})}{n}}} = \\frac{",p_sample," - ", p_pop,"}{\\sqrt{\\frac{", p_pop, "(1-", p_pop,")}{",n_sample,"}}} = ", round(testStatistics, 2),"\\)")}
            else if(testType ==  "one_var") {p("\\(", testSymbol,"_{hitung} = \\frac{(n-1)s^2}{\\sigma_{0}^{2}} = \\frac{(",n_sample," - 1)", var_sample,"}{", var_pop,"} = ", round(testStatistics, 2),"\\)")}
            else if(testType == "two_mean"){
              if(testVariables == "normal_test" | (testVariables == "t_test" && sd_assumption == "Not Equals")) {p("\\(", testSymbol,"_{hitung} = \\frac{(",statistik,") - (", parameter, ")}{\\sqrt{\\frac{",sdSymbol1,"}{n_1} + \\frac{",sdSymbol2,"}{n_2}}} = \\frac{(",mean1,"-", mean2,") - ", nilaiParam, "}{\\sqrt{\\frac{",sd1,"}{", n_sample1,"} + \\frac{",sd2,"}{",n_sample2,"}}} = ", round(testStatistics, 2),"\\)")}
              else {
                tagList(
                  p("\\(", testSymbol,"_{hitung} = \\frac{(",statistik,") - (", parameter, ")}{s_p\\sqrt{\\frac{1}{n_1} + \\frac{1}{n_2}}} = \\frac{(",mean1,"-", mean2,") - ", nilaiParam, "}{",round(sd_pulled, 2),"\\sqrt{\\frac{1}{", n_sample1,"} + \\frac{1}{",n_sample2,"}}} = ", round(testStatistics, 2),"\\)"),
                  p(""),
                  p("dimana:"),
                  p("\\(s_p = \\sqrt{\\frac{(n_1-1)s_1^2 + (n_2 -1)s_2^2}{n_1 + n_2 - 2}} \\)")
                )
              }
            }
            else if(testType == "two_prop"){
              if(assumption == "Equals"){
                tagList(
                  p("\\(", testSymbol,"_{hitung} = \\frac{(",statistik,")}{\\sqrt{\\hat{p}(1-\\hat{p})\\left(\\frac{1}{n_1} + \\frac{1}{n_2}\\right)}} = \\frac{(",p_sample1," - ", p_sample2,")}{\\sqrt{",round(p_pulled, 2),"(1 - ", round(p_pulled, 2),")\\left(\\frac{1}{",n_sample1,"} + \\frac{1}{",n_sample2,"}\\right)}} = ", round(testStatistics, 2),"\\)"),
                  p(""),
                  p("dimana:"),
                  p("\\(\\hat{p} = \\frac{x_1 + x_2}{n_1 + n_2} = \\frac{",n_interest1," + ",n_interest2,"}{",n_sample1," + ",n_sample2,"} = ", round(p_pulled, 2),"\\)")
                )
              } else {
                p("\\(", testSymbol,"_{hitung} = \\frac{(",statistik,") - (", parameter, ")}{\\sqrt{\\frac{\\hat{p_1}(1-\\hat{p_1})}{n_1} + \\frac{\\hat{p_2}(1-\\hat{p_2})}{n_2}}} = \\frac{(",p_sample1, " - ", p_sample2,") - (", p_diff, ")}{\\sqrt{\\frac{",p_sample1,"(1-",p_sample1,")}{",n_sample1,"} + \\frac{",p_sample1,"(1-",p_sample1,")}{",n_sample1,"}}} = ", round(testStatistics, 2),"\\)")
              }
            }
            else if(testType == "two_var"){
              p("\\(f = \\frac{s_1^2}{s_2^2} = \\frac{",var_sample1,"}{",var_sample2,"} = ", testStatistics,"\\)")
            }
          )
      )
    ),
    
    div(
      class = "step-result-card",
      div(class = "card-header-child", withMathJax("Step 5: Hasil dan Keputusan")),
      div(class = "card-body-child",
          div(style= "text-align: left;",
              p("Karena \\(", testSymbol,"_{hitung} \\) = ", round(testStatistics, 2), "berada di daerah ", keputusan, " (berada di dalam daerah", daerah, " \\(H_0 \\)), maka keputusannya adalah ", span(keputusan, style="font-weight: bold"))
          )
      )
    ),
    
    div(
      class = "step-result-card",
      div(class = "card-header-child", "Alternative Menggunakan P-Value"),
      div(class = "card-body-child",
          div(style= "text-align: left;",
              p("Jika p-value < \\(\\alpha\\), maka Tolak \\(H_0\\)"),
              p("Jika p-value > \\(\\alpha\\), maka Gagal Tolak \\(H_0\\)"),
              
              if(properTest == "Two-Tailed"){
                tagList(
                  p("p-value untuk uji dua arah:", style="font-weight: bold;"),
                  if(testVariables == "normal_test"){
                    tagList(
                      p("p-value = 2 \\(\\times\\) P(", testSymbol, "\\(\\leq\\) -|", round(testStatistics, 2), "|)"),
                      p("p-value = 2 \\(\\times\\) ", round(pnorm(testStatistics, mean = 0, sd=1), 4)),
                      p("p-value = ", p_value),
                    )
                  } else if(testVariables == "t_test"){
                    tagList(
                      p("p-value = 2 \\(\\times\\) P(", testSymbol, "\\(\\leq\\) -|", round(testStatistics, 2), "|)"),
                      p("p-value = 2 \\(\\times\\) ", round(pt(testStatistics, df= n_sample-1), 4)),
                      p("p-value = ", p_value),
                    )
                  } else if(testVariables == "chi_test"){
                    tagList(
                      p("p-Value = 2 \\(\\times \\min\\left( P(\\chi^2 \\leq", testStatistics, "), P(\\chi^2 \\geq ", testStatistics, ") \\right) \\)"),
                      p("p-value = 2 \\(\\times\\) ", round(min(pchisq(testStatistics, df = n_sample - 1), 1 - pchisq(testStatistics, df = n_sample - 1)), 4)),
                      p("p-value = ", p_value),
                    )
                  } else if(testVariables == "f_test"){
                    tagList(
                      p("p-Value = 2 \\(\\times \\min\\left( P(f \\leq", testStatistics, "), P(f \\geq ", testStatistics, ") \\right) \\)"),
                      p("p-value = 2 \\(\\times\\) ", round(min(pf(testStatistics, df1 = df1, df2 = df2), 1 - pf(testStatistics, df1 = df1, df2 = df2)), 4)),
                      p("p-value = ", p_value),
                    )
                  }
                )
              } else {
                if(properTest == "One-Tailed Left"){
                  simbol_p_value <- "\\(\\leq\\)"
                  tagList(
                    p("p-value = P(", testSymbol, simbol_p_value, round(testStatistics, 2), ")"),
                    p("p-value = ", p_value),
                  )
                }
                else {
                  simbol_p_value <- "\\(\\geq\\)"
                  tagList(
                    p("p-value = P(\\(\\", testSymbol, "\\)", simbol_p_value, round(testStatistics, 2), ")"),
                    p("p-value = ", p_value),
                  )
                }
              },
              
              if(p_value >= 1-conf_int){
                  p("Karena nilai \\(p-value (",p_value,") \\geq \\alpha (", 1-conf_int, ")\\), maka keputusan ", span("Gagal Tolak \\(H_0\\)", style="font-weight: bold"))
              } else{
                  p("Karena nilai \\(p-value (",p_value,") < \\alpha (", 1-conf_int, ")\\), maka keputusan ", span("Tolak \\(H_0\\)", style="font-weight: bold"))
              },
          )
      )
    ),
  )
}

######################################################################################################
#SERVER MODULE

# Module Server for Plot Distribution
renderDistributionPlot <- function(quantile, prob, lower_cv, upper_cv, testStatistics, plotTitle) {
  ggplot(data = data.frame(quantile, prob), aes(x = quantile, y = prob)) +
    geom_line(color = "#4D89B0", size = 1.5) +  
    
    # Area kritis dengan gradasi warna yang halus
    geom_area(data = subset(data.frame(quantile, prob), !is.na(lower_cv) & quantile < lower_cv), 
              aes(x = quantile, y = prob), fill = "#F9A8B1", alpha = 0.4) +  
    geom_area(data = subset(data.frame(quantile, prob), !is.na(upper_cv) & quantile > upper_cv), 
              aes(x = quantile, y = prob), fill = "#F9A8B1", alpha = 0.4) + 
    
    # Garis vertikal untuk nilai kritis dengan warna yang lebih lembut
    { if (!is.na(lower_cv)) geom_vline(xintercept = lower_cv, linetype = "dashed", color = "#D04A7D", size = 1.2) } +  # Merah muda gelap
    { if (!is.na(upper_cv)) geom_vline(xintercept = upper_cv, linetype = "dashed", color = "#D04A7D", size = 1.2) } +  # Merah muda gelap
    
    # Garis vertikal untuk statistik uji
    geom_vline(xintercept = testStatistics, linetype = "solid", color = "#1A3A5B", size = 1.5) +  # Biru tua
    
    # Anotasi dengan teks lebih elegan dan bold untuk TS dan CV
    { if (!is.na(lower_cv)) annotate("text", x = lower_cv, y = max(prob)*0.9, 
                                     label = paste("CV =", round(lower_cv, 2)), 
                                     hjust = 1.1, size = 5, color = "#D04A7D", fontface = "bold") } +  # Bold untuk CV
    { if (!is.na(upper_cv)) annotate("text", x = upper_cv, y = max(prob)*0.9, 
                                     label = paste("CV =", round(upper_cv, 2)), 
                                     hjust = -0.1, size = 5, color = "#D04A7D", fontface = "bold") } +  # Bold untuk CV
    annotate("text", x = testStatistics, y = max(prob)*0.7, 
             label = paste("TS =", round(testStatistics, 2)), hjust = -0.1, size = 5, 
             color = "#1A3A5B", fontface = "bold") + 
    
 
    labs(title = plotTitle,
         x = "Test Statistic",
         y = "Probability Density") +
    theme_minimal(base_size = 14) +  
    theme(
      panel.grid.major = element_line(color = "gray90", size = 0.5),  
      panel.grid.minor = element_blank(),  
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#1A3A5B"),
      axis.title.x = element_text(size = 14, face = "bold", color = "#1A3A5B"),
      axis.title.y = element_text(size = 14, face = "bold", color = "#1A3A5B"),
      axis.text = element_text(size = 12, color = "#1A3A5B")
    )
}


# Module Server One Population Mean Test
onePopMeanTest_server <- function(id, mu, mean, sd, n_sample, sd_Type, conf_int, calculate, properTest) {
  moduleServer(id, function(input, output, session) {
    
    result_reactive <- reactiveVal(NULL)
    
    observeEvent(calculate(), {  # Ensure button click triggers
      df <- n_sample() - 1
      testStatistics <- (mean() - mu()) / (sd() / sqrt(n_sample()))
      
      alpha <- 1 - conf_int()
      
      switch (properTest(),
              "Two-Tailed" = {
                critical_values <- if (sd_Type() == "Population" || n_sample() >= 30) {
                  critical_val <- qnorm(1 - alpha / 2)
                  c(-critical_val, critical_val)
                } else {
                  critical_val <- qt(1 - alpha / 2, df)
                  c(-critical_val, critical_val)
                }
                
                testVariables <- if (sd_Type() == "Population" || n_sample() >= 30) {
                  "normal_test"
                } else {
                  "t_test"
                }
                
                lower_cv = critical_values[1]
                upper_cv = critical_values[2]
              },
              
              "One-Tailed Left" = {
                lower_cv <- if (sd_Type() == "Population" || n_sample() >= 30) {
                  critical_val <- qnorm(1 - alpha)
                  c(-critical_val)
                } else {
                  critical_val <- qt(1 - alpha, df)
                  c(-critical_val)
                }
                
                testVariables <- if (sd_Type() == "Population" || n_sample() >= 30) {
                  "normal_test"
                } else {
                  "t_test"
                }
                
                upper_cv = NA
              },
              
              "One-Tailed Right" = {
                upper_cv <- if (sd_Type() == "Population" || n_sample() >= 30) {
                  critical_val <- qnorm(1 - alpha)
                  c(critical_val)
                } else {
                  critical_val <- qt(1 - alpha, df)
                  c(critical_val)
                }
                
                testVariables <- if (sd_Type() == "Population" || n_sample() >= 30) {
                  "normal_test"
                } else {
                  "t_test"
                }
                
                lower_cv = NA
              }
      )
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      
      prob <- if (sd_Type() == "Population" || n_sample() > 30) {
        dnorm(quantile, mean = 0, sd = 1)
      } else {
        dt(quantile, df)
      }
      
      output$distributionPlot <- renderPlot({
        renderDistributionPlot(
          quantile = quantile,
          prob = prob,
          lower_cv = lower_cv,
          upper_cv = upper_cv,
          testStatistics = testStatistics,
          plotTitle = "Hypothesis Testing for Population Mean"
        )
      })
      
      result_reactive(list(
        properTest = properTest(),
        mu = mu(),
        conf_int = conf_int(),
        mean = mean(),
        n_sample = n_sample(),
        sd = sd()
      ))
      
      output$stepResult <- renderUI({
        req(result_reactive())
        renderStepResult(
          properTest = result_reactive()$properTest,
          mu = result_reactive()$mu,
          conf_int = result_reactive()$conf_int,
          testStatistics = testStatistics,
          lower_cv = round(lower_cv, 2),
          upper_cv = round(upper_cv, 2),
          mean = result_reactive()$mean,
          n_sample = result_reactive()$n_sample,
          sd = result_reactive()$sd,
          testType = "one_mean",
          testVariables = testVariables
        )
      })
    })
  })
}

# Module Server Two Population Mean Test
twoPopMeanTest_server <- function(id, mu_dif, mean1, mean2, sd1, sd2, n_sample1, n_sample2, sd_Type, conf_int, calculate, properTest, sd_assumption) {
  moduleServer(id, function(input, output, session) {
    result_reactive <- reactiveVal(NULL)
    
    observeEvent(calculate(), {  # Ensure button click triggers
      sd_pulled <- NA
      
      df <- if(sd_Type() == "Sample"){
        if(sd_assumption() == "Equals"){
          (n_sample1() + n_sample2() - 2)
        } else {
          (sd1()^2/n_sample1() + sd2()^2/n_sample2())^2/(((sd1()^2/n_sample1())^2/(n_sample1()-1)) + ((sd2()^2/n_sample2())^2/(n_sample2() - 1)))
        }
      } 
      
      testStatistics <- if(sd_Type() == "Population") {
        ((mean1()-mean2()) - mu_dif())/sqrt((sd1()^2/n_sample1()) + (sd2()^2/n_sample2()))
        
      } else {
        if(sd_assumption() == "Equals"){
          sd_pulled <- sqrt(((n_sample1() - 1)*sd1()^2 + (n_sample2() - 1)*sd2()^2)/(n_sample1() + n_sample2() - 2))
          
          ((mean1()-mean2()) - mu_dif())/(sd_pulled*sqrt((1/n_sample1()) + (1/n_sample2())))
        } else {
          ((mean1()-mean2()) - mu_dif())/sqrt((sd1()^2/n_sample1()) + (sd2()^2/n_sample2()))
        }
      }
      
      testVariables <- if(sd_Type() == "Population") {
        "normal_test"
      } else {
        "t_test"
      }
      
      alpha <- 1 - conf_int()
      
      switch (properTest(),
              "Two-Tailed" = {
                critical_values <- if (sd_Type() == "Population") {
                  critical_val <- qnorm(1 - alpha / 2)
                  c(-critical_val, critical_val)
                  
                } else {
                  critical_val <- qt(1 - alpha / 2, df)
                  c(-critical_val, critical_val)
                }
                
                lower_cv = critical_values[1]
                upper_cv = critical_values[2]
              },
              
              "One-Tailed Left" = {
                lower_cv <- if (sd_Type() == "Population") {
                  critical_val <- qnorm(1 - alpha)
                  c(-critical_val)
                } else {
                  critical_val <- qt(1 - alpha, df)
                  c(-critical_val)
                }
                
                upper_cv = NA
              },
              
              "One-Tailed Right" = {
                upper_cv <- if (sd_Type() == "Population") {
                  critical_val <- qnorm(1 - alpha)
                  c(critical_val)
                } else {
                  critical_val <- qt(1 - alpha, df)
                  c(critical_val)
                }
                
                lower_cv = NA
              }
      )
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      
      prob <- if (sd_Type() == "Population") {
        dnorm(quantile, mean = 0, sd = 1)
      } else {
        dt(quantile, df)
      }
      
      output$distributionPlot <- renderPlot({
        renderDistributionPlot(
          quantile = quantile,
          prob = prob,
          lower_cv = lower_cv,
          upper_cv = upper_cv,
          testStatistics = testStatistics,
          plotTitle = "Hypothesis Testing for Population Mean"
        )
      })
      
      result_reactive(list(
        mu_dif = mu_dif(), 
        mean1 = mean1(), 
        mean2= mean2(), 
        sd1= sd1(), 
        sd2= sd2(), 
        n_sample1= n_sample1(), 
        n_sample2= n_sample2(), 
        sd_Type = sd_Type(), 
        conf_int = conf_int(), 
        properTest = properTest(), 
        sd_assumption = sd_assumption(),
        testStatistics = testStatistics,
        lower_cv = lower_cv,
        upper_cv = upper_cv,
        testVariables = testVariables
      ))
      
      output$stepResult <- renderUI({
        req(result_reactive())
        renderStepResult(
          mu_dif = result_reactive()$mu_dif, 
          mean1 = result_reactive()$mean1, 
          mean2= result_reactive()$mean2, 
          sd1= result_reactive()$sd1, 
          sd2= result_reactive()$sd2, 
          n_sample1= result_reactive()$n_sample1, 
          n_sample2= result_reactive()$n_sample2, 
          sd_Type = result_reactive()$sd_Type, 
          conf_int = result_reactive()$conf_int, 
          properTest = result_reactive()$properTest, 
          sd_assumption = result_reactive()$sd_assumption,
          testStatistics = result_reactive()$testStatistics,
          lower_cv = round(result_reactive()$lower_cv, 2),
          upper_cv = round(result_reactive()$upper_cv, 2),
          testType = "two_mean",
          testVariables = testVariables,
          df = df,
          sd_pulled = sd_pulled
        )
      })
    })
  })
}

# Module Server One Population Proportion Test
onePopPropTest_server <- function(id, p_pop, p_sample, n_sample, conf_int, calculate, properTest){
  moduleServer(id, function(input, output, session){
    
    result_reactive <- reactiveVal(NULL)
    
    observeEvent(calculate(), {
      testStatistics <- (p_sample() - p_pop())/(sqrt((p_pop()*(1-p_pop()))/n_sample()))
      
      alpha <- 1 - conf_int()
      
      lower_cv <- NA 
      upper_cv <- NA  
      
      switch (properTest(),
              "Two-Tailed" = {
                critical_values <- qnorm(1 - alpha / 2)
                cv <- c(-critical_values, critical_values)
                
                lower_cv = cv[1]
                upper_cv = cv[2]
              },
              
              "One-Tailed Left" = {
                lower_cv <- -(qnorm(1 - alpha))
              },
              
              "One-Tailed Right" = {
                upper_cv <- qnorm(1 - alpha)
              }
      )
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      
      prob <- dnorm(quantile, mean = 0, sd = 1)
      
      output$distributionPlot <- renderPlot({
        renderDistributionPlot(
          quantile = quantile,
          prob = prob,
          lower_cv = lower_cv,
          upper_cv = upper_cv,
          testStatistics = testStatistics,
          plotTitle = "Hypothesis Testing for Population Proportion"
        )
      })
      
      result_reactive(list(
        properTest = properTest(),
        p_pop = p_pop(),
        p_sample = p_sample(),
        conf_int = conf_int(),
        n_sample = n_sample(),
        testStatistics = testStatistics,
        lower_cv = lower_cv,
        upper_cv = upper_cv
      ))
      
      output$stepResult <- renderUI({
        req(result_reactive())
        renderStepResult(
          properTest = result_reactive()$properTest,
          p_pop = result_reactive()$p_pop,
          p_sample = result_reactive()$p_sample,
          conf_int = result_reactive()$conf_int,
          testStatistics = result_reactive()$testStatistics,
          lower_cv = round(result_reactive()$lower_cv, 2),
          upper_cv = round(result_reactive()$upper_cv, 2),
          n_sample = result_reactive()$n_sample,
          testType = "one_prop",
          testVariables = "normal_test"
        )
      })
    })
  })
}


# Module Server Two Population Proportion Test
twoPopPropTest_server <- function(id, p_diff, n_interest1, n_interest2, n_sample1, n_sample2, conf_int, calculate, properTest, assumption){
  moduleServer(id, function(input, output, session){
    
    result_reactive <- reactiveVal(NULL)
    
    observeEvent(calculate(), {
      p_pulled <- NA
      p_sample1 <- n_interest1()/n_sample1()
      p_sample2 <- n_interest2()/n_sample2()
      
      testStatistics <- if(assumption() == "Equals"){
        p_pulled <- (n_interest1() + n_interest2())/(n_sample1() + n_sample2())
        
        ((p_sample1 - p_sample2)/sqrt(p_pulled*(1-p_pulled)*(1/n_sample1() + 1/n_sample2())))
        
      } else {
        ((p_sample1-p_sample2) - (p_diff()))/sqrt((p_sample1*(1-p_sample1))/n_sample1() + (p_sample2*(1-p_sample2))/n_sample2())
      }
      
      
      alpha <- 1 - conf_int()
      
      switch (properTest(),
              "Two-Tailed" = {
                critical_values <- qnorm(1 - alpha / 2)
                cv <- c(-critical_values, critical_values)
                
                lower_cv = cv[1]
                upper_cv = cv[2]
              },
              
              "One-Tailed Left" = {
                lower_cv <- -(qnorm(1 - alpha))
                upper_cv = NA
              },
              
              "One-Tailed Right" = {
                upper_cv <- qnorm(1 - alpha)
                lower_cv = NA
              }
      )
      
      quantile <- seq(-3.5, 3.5, length = 1000)
      
      prob <- dnorm(quantile, mean = 0, sd = 1)
      
      output$distributionPlot <- renderPlot({
        renderDistributionPlot(
          quantile = quantile,
          prob = prob,
          lower_cv = lower_cv,
          upper_cv = upper_cv,
          testStatistics = testStatistics,
          plotTitle = "Hypothesis Testing for Population Proportion"
        )
      })
      
      
      result_reactive(list(
        properTest = properTest(),
        p_diff = p_diff(),
        p_sample1 = p_sample1,
        p_sample2 = p_sample1,
        conf_int = conf_int(),
        n_sample1 = n_sample1(),
        n_sample2 = n_sample2(),
        n_interest1 = n_interest1(),
        n_interest2 = n_interest2(),
        testStatistics = testStatistics,
        lower_cv = round(lower_cv, 2),
        upper_cv = round(upper_cv, 2),
        assumption = assumption()
      ))
      
      output$stepResult <- renderUI({
        req(result_reactive())
        renderStepResult(
          properTest = result_reactive()$properTest,
          p_diff = result_reactive()$p_diff,
          p_sample1 = result_reactive()$p_sample1,
          p_sample2 = result_reactive()$p_sample1,
          conf_int = result_reactive()$conf_int,
          n_sample1 = result_reactive()$n_sample1,
          n_sample2 = result_reactive()$n_sample2,
          n_interest1 = result_reactive()$n_interest1,
          n_interest2 = result_reactive()$n_interest2,
          testStatistics = result_reactive()$testStatistics,
          lower_cv = result_reactive()$lower_cv,
          upper_cv = result_reactive()$upper_cv,
          assumption = result_reactive()$assumption,
          testType = "two_prop",
          testVariables = "normal_test",
          p_pulled = p_pulled
        )
      })
    })
  })
}

# Module Server One Population variance Test
onePopVarTest_server <- function(id, var_pop, var_sample, n_sample, conf_int, calculate, properTest) {
  moduleServer(id, function(input, output, session){
    
    result_reactive <- reactiveVal(NULL)
    
    observeEvent(calculate(), {
      df <- n_sample() - 1
      testStatistics <- ((n_sample() - 1)* var_sample()) / (var_pop())
      
      alpha <- 1 - conf_int()
      
      switch(properTest(),
       "Two-Tailed" = {
         lower_cv <- qchisq(alpha / 2, df, lower.tail = T)
         upper_cv <- qchisq(alpha / 2, df, lower.tail = F)
       },
       "One-Tailed Left" = {
         lower_cv <- qchisq(alpha, df, lower.tail = T)
         upper_cv <- NA
       },
       "One-Tailed Right" = {
         upper_cv <- qchisq(alpha, df, lower.tail = F)
         lower_cv <- NA
       }
      )
      
      # Dynamic quantile range
      quantile <- seq(0, qchisq(0.999, df), length = 1000)
      prob <- dchisq(quantile, df)
      
      output$distributionPlot <- renderPlot({
        renderDistributionPlot(
          quantile = quantile,
          prob = prob,
          lower_cv = lower_cv,
          upper_cv = upper_cv,
          testStatistics = testStatistics,
          plotTitle = "Hypothesis Testing for Population Variance"
        )
      })
      
      result_reactive(list(
        properTest = properTest(),
        var_pop = var_pop(),
        var_sample = var_sample(),
        conf_int = conf_int(),
        n_sample = n_sample(),
        testStatistics = testStatistics,
        lower_cv = lower_cv,
        upper_cv = upper_cv
      ))
      
      output$stepResult <- renderUI({
        req(result_reactive())
        renderStepResult(
          properTest = result_reactive()$properTest,
          var_pop = result_reactive()$var_pop,
          var_sample = result_reactive()$var_sample,
          conf_int = result_reactive()$conf_int,
          testStatistics = result_reactive()$testStatistics,
          lower_cv = round(result_reactive()$lower_cv, 2),
          upper_cv = round(result_reactive()$upper_cv, 2),
          n_sample = result_reactive()$n_sample,
          testType = "one_var",
          testVariables = "chi_test"
        )
      })
    })
  })
}

# Module Server Two Population variance Test
twoPopVarTest_server <- function(id, var_sample1, var_sample2, n_sample1, n_sample2, conf_int, calculate, properTest) {
  
  result_reactive <- reactiveVal(NULL)
  
  moduleServer(id, function(input, output, session){
    observeEvent(calculate(), {
      df1 <- n_sample1() - 1
      df2 <- n_sample2() - 1
      
      testStatistics <- var_sample1()/var_sample2()
      
      alpha <- 1 - conf_int()
      
      switch(properTest(),
             "Two-Tailed" = {
               lower_cv <- qf(alpha / 2, df1 = df1, df2 = df2, lower.tail = T)
               upper_cv <- qf(alpha / 2, df1 = df1, df2 = df2, lower.tail = F)
             },
             "One-Tailed Left" = {
               lower_cv <- qf(alpha, df1 = df1, df2 = df2, lower.tail = T)
               upper_cv <- NA
             },
             "One-Tailed Right" = {
               upper_cv <- qf(alpha, df1 = df1, df2 = df2, lower.tail = F)
               lower_cv <- NA
             }
      )
      
      # Dynamic quantile range
      lower_bound <- 0
      upper_bound <- max(testStatistics, upper_cv, na.rm = TRUE) * 1.5
      quantile <- seq(lower_bound, upper_bound, length.out = 1000)
      
      prob <- df(quantile, df1 = df1, df2 = df2)
      prob <- df(quantile, df1 = df1, df2 = df2)
      
      output$distributionPlot <- renderPlot({
        renderDistributionPlot(
          quantile = quantile,
          prob = prob,
          lower_cv = lower_cv,
          upper_cv = upper_cv,
          testStatistics = testStatistics,
          plotTitle = "Hypothesis Testing for Population Variance"
        )
      })
      
      result_reactive(list(
        properTest = properTest(),
        var_sample1 = var_sample1(),
        var_sample2 = var_sample2(),
        conf_int = conf_int(),
        n_sample1 = n_sample2(),
        n_sample1 = n_sample2(),
        testStatistics = testStatistics,
        lower_cv = round(lower_cv, 2),
        upper_cv = round(upper_cv, 2)
      ))
      
      output$stepResult <- renderUI({
        req(result_reactive())
        renderStepResult(
          properTest = result_reactive()$properTest,
          var_sample1 = result_reactive()$var_sample1,
          var_sample2 = result_reactive()$var_sample2,
          conf_int = result_reactive()$conf_int,
          testStatistics = result_reactive()$testStatistics,
          lower_cv = round(result_reactive()$lower_cv, 2),
          upper_cv = round(result_reactive()$upper_cv, 2),
          n_sample1 = result_reactive()$n_sample1,
          n_sample2 = result_reactive()$n_sample2,
          testType = "two_var",
          testVariables = "f_test",
          df1 = df1,
          df2 = df2
        )
      })
    })
  })
}

####################################################################################################
# MAIN UI
ui <- fluidPage(
  
  withMathJax(),
  
  # Menambahkan CSS
  tags$head(
    tags$style(HTML("
    @keyframes slideInFromLeft {
      0% {
        transform: translateX(-100%); 
        opacity: 0; 
      }
      100% {
        transform: translateX(0); 
        opacity: 1;
      }
    }

    @keyframes slideInFromTop {
      0% {
        transform: translateY(-100%); 
        opacity: 0; 
      }
      100% {
        transform: translateY(0); 
        opacity: 1; 
      }
    }
    
    @keyframes slideInFromBottom {
      0% {
        transform: translateY(100%); 
        opacity: 0;
      }
      100% {
        transform: translateY(0);
        opacity: 1; 
      }
    }
    
    @keyframes fadeIn {
      0% {
        opacity: 0; 
        transform: translateY(-20px); 
      }
      100% {
        opacity: 1; 
        transform: translateY(0); 
      }
    }
    
    @keyframes slideInFromRight {
      0% {
        transform: translateX(100%); 
        opacity: 0; 
      }
      100% {
        transform: translateX(0); 
        opacity: 1;
      }
    }
    
      body {
        background-image: url('hero-bg-abstract.jpg');
        background-color: rgba(255,255,255,0.85);
        background-blend-mode: lighten;
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        height: 100vh; 
      }
    
      .card{
        margin: 0px;
        padding: 0px;
        font-family:'Verdana';
        background-image: url('www/rb_59421.png'); 
        background-size: cover;
      }
      
      .container-fluid {
        margin: 0px;
        padding: 0px
      }
      
      .header_row{
        padding: 40px 25px;
        display: flex;
        justify-content: center;
        align-items: center;
        height: 60px;
        font-size: 16px;
        background-color: #0E3E87;
      }
      
      .header_row_title {
        font-size: 25px;
        font-family: 'Arial Black';
      }
      
      .title_1 {
        background-color:#ffffff;
        border-radius: 8px;
        padding: 6px 10px 6px 15px;
        color: #0E3E87;
      }
      
      .title_2 {
        color: #FFF2F4;
      }
      
      .header_tab{
        display:flex;
        flex:60%;
        justify-content: center;
        align-items: center;
        
      }
      
      .github_tab{
        display: flex;
        flex:8%;
        justify-content: right;
      }
      
      .header_icon{
        font-size: 25px;
      }
      
      .header_icon_text{
        font-weight: bold;
      }
      
      .github_icon{
        font-size: 25px;
        color: #0E3E87;
      }
      
      .github_icon:hover{
        color: #004683;
      }
  
      .home_nav_tab {
        font-size: 10px;
        color: #ffffff;
        margin: 0px 20px;
        position: relative;
        cursor: pointer;
        border: none;
        box-shadow: none !important;
        background-color: transparent;
      }
  
      .home_nav_tab::before {
          content: '';
          position: absolute;
          bottom: 0px;
          left: 0px;
          height: 2px;
          width: 100%;
          background-color: #ffffff;
          border-radius: 50px;
          transform: scaleX(0);
          transition: background-color 0.3s ease-in-out;
          transition: transform 0.3s ease-in-out;
      }

      .home_nav_tab.active {
        background-color: transparent !important;
        color: #ffffff;
        box-shadow: none !important;
        border:none;
      }
      
      .home_nav_tab:hover {
        background-color: transparent !important;
        color: #1977cc;
      }

      .home_nav_tab:hover::before,
      .home_nav_tab.active::before {
        transform: scaleX(1);
      }
      
      .home_nav:hover{
        background-color: #1750A5;
        color: #ECECEC;
      }
      
      .home_nav {
        padding-top: 100px;
        padding-bottom: 100px;
        display: flex;
        color: #ECECEC;
        background: linear-gradient(to bottom, #1250ab, #073964);
        justify-content: center;
        flex:1;
        padding: 10px;
        border-radius: 0px;
        transition: background-color 0.5s ease-in-out;
        border: none;
      }
      
      .step-result-card{
        border-radius: 8px;
      }
      
      #two_pop_panel:active, #one_pop_panel:active {
        background-color: transparent !important;
        color: #1977cc;
      }
      
      .card{
        border: 1px solid #ddd;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        margin-bottom: 20px;
      }
      
      .card-parent {
        animation: slideInFromBottom 0.8s ease-out forwards; 
        margin-bottom: 20px;
      }
      
      .card-header-parent {
        
        background-color: #0E3E87;
        color: #FFF2F4;
        font-weight: bold;
        padding: 10px;
        border-radius: 8px;
      }
      
      .card-header-child {
        background-color: #3588D1;
        color: #FFF2F4;
        font-weight: bold;
        padding: 10px;
        border-radius: 8px;
        animation: slideInFromTop 0.8s ease-out forwards;
      }

      .card-body-parent {
        padding: 15px;
        background-color: #FAFDFF;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        }
      
      .card-body-child {
        padding: 20px 50px;
        background-color:#FFFFFF;
        animation: slideInFromTop 0.8s ease-out forwards;
      }
      
      .sidebarPanel {
        padding-top: 35px;
        background-color: #FAFDFF;
        color: #10428D;
        animation: slideInFromTop 0.8s ease-out forwards; 
      }
      
      .mainPanel {
        padding-top: 30px
      }
      
      .profile-card {
        width: 300px;
        background-color: #ffffff;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        overflow: hidden;
        text-align: center;
        opacity: 0;
        animation: slideInFromTop 0.6s ease-out forwards;
      }

      .profile-card:hover {
        transform: translateY(-10px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
      }

      .profile-card-header {
        background-color: #10428d;
        padding: 20px;
      }

      .profile-picture {
        width: 200px; /* Ukuran lebih besar */
        height: 200px; /* Ukuran lebih besar */
        border-radius: 50%;
        border: 4px solid #ffffff;
        object-fit: cover;
        object-position: center 20%;
      }


      .profile-card-body {
        padding: 20px;
      }

      .name {
        font-size: 1.5rem;
        color: #10428d;
        margin: 10px 0 5px;
      }

      .role {
        font-size: 1rem;
        color: #6c757d;
        margin: 5px 0;
      }

      .bio {
        font-size: 0.9rem;
        color: #333333;
        margin: 10px 0;
        line-height: 1.5;
      }

      .social-links a {
        display: inline-block;
        margin: 5px;
        text-decoration: none;
        color: #10428d;
        font-size: 0.9rem;
        padding: 5px 10px;
        border: 1px solid #10428d;
        border-radius: 5px;
        transition: background-color 0.3s ease, color 0.3s ease;
      }

      .social-links a:hover {
        background-color: #10428d;
        color: #ffffff;
      }

      .hidden {
        display: none;
      }
      
      container-card {
        display: flex;
        flex-direction: column;
        align-items: center;
        margin: 20px auto;
      }
      .row-card {
        display: flex;
        justify-content: center;
        margin-bottom: 20px;
      }
      .col-card {
        margin: 10px;
        display: flex;
        justify-content: center;
      }
      
      .Welcoming-container {
        padding-left: 250px;
        padding-right: 250px;
        margin-top: 10px;
        display: flex;
        align-items: center;
        background-color: transparent;
      }
      
      @media (max-width: 1100px) {
        .Welcoming-container {
          padding-left: 20px;
          padding-right: 20px;
        }
      }
      
      .welcome {
        background: linear-gradient(to right, #0E3E87, #3489D4);
        flex: 50%;
        text-align: left;
        padding: 20px;
        padding-left: 70px;
        font-size: 4.5rem;
        color: #FFFFFF;
        font-weight: bold;
        text-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3);
        opacity: 0; 
        animation: slideInFromLeft 1.5s ease-out forwards; 
      }
      
      .welcom-guide {
        background: linear-gradient(to left, #F2F9FD, #D0E4F1);
        flex: 50%;
        padding: 20px;
        padding-right: 70px;
        padding-top: 42px;
        padding-bottom: 42px;
        text-align: right;
        font-size: 1.5rem;
        color: #0C346F;
        opacity: 0; 
        animation: slideInFromRight 1.5s ease-out forwards; 
      }
      .welcoming-title{
        margin-top: 60px;
        text-align:center;
        font-size: 4.5rem;
        color: #0C346F; 
        font-weight: bold;
      }
      
      .welcoming-title {
        margin-top: 60px;
        text-align: center;
        font-size: 4.5rem;
        color: #0C346F;
        font-weight: bold;
        animation: fadeIn 2s ease-out forwards;
    }
    
    .calculate-btn {
      background-color: #3489D4; 
      color: white; 
      font-size: 14px; 
      font-weight: bold;
      padding: 8px 16px; 
      border-radius: 5px;
      border: none; 
      cursor: pointer; 
      transition: background-color 0.3s ease, transform 0.3s ease;
    }
    
    .calculate-btn:hover {
      background-color: #76B1E5;
      color: #B9D7F2;
    }

    "))
  ),
  
  # Menambahkan HTML
  tags$script(HTML("
    MathJax.Hub.Config({
      displayAlign: 'left'  
    });
    
    document.addEventListener('DOMContentLoaded', function() {
      document.querySelectorAll('.home_nav_tab').forEach(function(tab) {
        tab.addEventListener('click', function() {
          document.querySelectorAll('.home_nav_tab').forEach(function(otherTab) {
            otherTab.classList.remove('active'); 
          });
          this.classList.add('active'); 
        });
      });
    });
  ")),
  
  div(class = "header_row",
      div(class = "header_row_title", span(class="title_1","Hypothesis"), span(class="title_2","Test")),
      div(class = "header_tab",
          actionButton(class="home_nav_tab", inputId = "home", div(icon("house", class = "fa-solid fa-user header_icon"), div("Home", class="header_icon_text"))),
          actionButton(class="home_nav_tab", inputId = "about_us", div(icon("users", class="header_icon"), div("About Us", class="header_icon_text")))
      ),
      div(class= "github_tab",
          actionButton("github_link", icon("github", class="github_icon"), onclick="window.open('https://github.com/Nakhaa/Final_Project_Shiny_App.git')")
      ),
  ),
  
  bsTooltip("github_link", "source code", placement = "bottom"),
  
  uiOutput("home_nav_ui"),
  
  
  sidebarLayout(
    uiOutput("home_tab_ui"),
    mainPanel(
    )
  )
)

######################################################################################################
# MAIN SERVER

server <- function(input, output, session) {
  useShinyjs()
  
  output$home_nav_ui <- renderUI(render_homeNav_ui("homeNav"))
  
  observeEvent(input$about_us, {
    output$home_nav_ui <- renderUI(render_aboutInfo_ui("about_info"))
    output$home_tab_ui <- renderUI(NULL)
    output$sidebarInput_ui <- renderUI(
      NULL
    )
  });
  
  output$home_tab_ui <- renderUI(render_homeTab_ui(id = "one_pop_panel", type = "one"))
  
  observeEvent(input$one_pop_test, {
    updateTabsetPanel(inputId= "popTest1", selected = input$one_pop_test)
  })
  
  output$sidebarInput_ui <- renderUI(
    sidebarInput_ui(id= "one_pop", type = "One Population")
  )
  # Calculation module
  observeEvent(input$calculate1, {
    if(!is.null(input$one_pop_test) && length(input$one_pop_test) == 1){
      switch(input$one_pop_test,
             "Mean Test" = {
               onePopMeanTest_server("meanTest",
                                     mu = reactive(input$mu),
                                     mean = reactive(input$mean),
                                     sd = reactive(input$sd),
                                     n_sample = reactive(input$n_mean),
                                     sd_Type = reactive(input$sd_Type1),
                                     conf_int = reactive(input$conf_int),
                                     calculate = reactive(input$calculate1),
                                     properTest = reactive(input$properTest)
               )
               
               output$dynamic_plot1 <- renderUI(distributionPlot_ui("meanTest"))
               output$step_result1 <- renderUI(stepResult_ui("meanTest"))
             },
             
             "Proportion Test" = {
               onePopPropTest_server("propTest",
                                     p_pop = reactive(input$p_pop),
                                     p_sample = reactive(input$p_sample),
                                     n_sample = reactive(input$n_prop),
                                     conf_int = reactive(input$conf_int),
                                     calculate = reactive(input$calculate1),
                                     properTest = reactive(input$properTest)
               )
               
               output$dynamic_plot1 <- renderUI(distributionPlot_ui("propTest"))
               output$step_result1 <- renderUI(stepResult_ui("propTest"))
             },
             
             "Variance Test" = {
               onePopVarTest_server("varTest",
                                    var_pop = reactive(input$var_pop),
                                    var_sample = reactive(input$var_sample),
                                    n_sample = reactive(input$n_var),
                                    conf_int = reactive(input$conf_int),
                                    calculate = reactive(input$calculate1),
                                    properTest = reactive(input$properTest)
               )
               
               output$dynamic_plot1 <- renderUI(distributionPlot_ui("varTest"))
               output$step_result1 <- renderUI(stepResult_ui("varTest"))
             },
      )
    }
  })
  
  observeEvent(input$one_pop_panel, {
    
    output$home_tab_ui <- renderUI(render_homeTab_ui(id = "one_pop_panel", type = "one"))
    
    observeEvent(input$one_pop_test, {
      updateTabsetPanel(inputId= "popTest1", selected = input$one_pop_test)
    })
    
    observeEvent(input$one_pop_panel, {
      output$sidebarInput_ui <- renderUI(
        sidebarInput_ui(id= "one_pop", type = "One Population")
      )
    })
    
    # Calculation module
    observeEvent(input$calculate1, {
      if(!is.null(input$one_pop_test) && length(input$one_pop_test) == 1){
        switch(input$one_pop_test,
               "Mean Test" = {
                 onePopMeanTest_server("meanTest",
                                       mu = reactive(input$mu),
                                       mean = reactive(input$mean),
                                       sd = reactive(input$sd),
                                       n_sample = reactive(input$n_mean),
                                       sd_Type = reactive(input$sd_Type1),
                                       conf_int = reactive(input$conf_int),
                                       calculate = reactive(input$calculate1),
                                       properTest = reactive(input$properTest)
                 )
                 
                 output$dynamic_plot1 <- renderUI(distributionPlot_ui("meanTest"))
                 output$step_result1 <- renderUI(stepResult_ui("meanTest"))
               },
               
               "Proportion Test" = {
                 onePopPropTest_server("propTest",
                                       p_pop = reactive(input$p_pop),
                                       p_sample = reactive(input$p_sample),
                                       n_sample = reactive(input$n_prop),
                                       conf_int = reactive(input$conf_int),
                                       calculate = reactive(input$calculate1),
                                       properTest = reactive(input$properTest)
                 )
                 
                 output$dynamic_plot1 <- renderUI(distributionPlot_ui("propTest"))
                 output$step_result1 <- renderUI(stepResult_ui("propTest"))
               },
               
               "Variance Test" = {
                 onePopVarTest_server("varTest",
                                      var_pop = reactive(input$var_pop),
                                      var_sample = reactive(input$var_sample),
                                      n_sample = reactive(input$n_var),
                                      conf_int = reactive(input$conf_int),
                                      calculate = reactive(input$calculate1),
                                      properTest = reactive(input$properTest)
                 )
                 
                 output$dynamic_plot1 <- renderUI(distributionPlot_ui("varTest"))
                 output$step_result1 <- renderUI(stepResult_ui("varTest"))
               },
        )
      }
    })
  })
  
  observeEvent(input$two_pop_panel, {
    output$home_tab_ui <- renderUI(render_homeTab_ui(id = "two_pop_panel", type = "two"))
    
    observeEvent(input$two_pop_test, {
      updateTabsetPanel(inputId = "popTest2", selected = input$two_pop_test)
    })
    
    observeEvent(input$two_pop_panel, {
      output$sidebarInput_ui <- renderUI(
        sidebarInput_ui(id = "two_pop", type = "Two Population")
      )
    })
    
    # for two population
    observeEvent(input$prop_assumption, {
      if(input$prop_assumption == "Equals") {
        output$prop_params <- renderUI({
          NULL
        })
      } else {
        output$prop_params <- renderUI({
          tagList(
            numericInput("p_diff", "Population Proportion Difference", value = 0.08),
          )
        })
      }
    })
    
    observeEvent(input$sd_Type2, {
      if(input$sd_Type2 == "Sample") {
        output$sd_assumption <- renderUI({
          selectInput("sd_assumption", "Assumption for Population SD", choices = param_assumption)
        })
      } else {
        output$sd_assumption <- renderUI({NULL})
      }
    })
    
    observeEvent(input$calculate2, {
      if(!is.null(input$two_pop_test) && length(input$two_pop_test) == 1){
        switch(input$two_pop_test,
               "Mean Test" = {
                 twoPopMeanTest_server("meanTest2",
                                       mu_dif = reactive(input$mu_dif),
                                       mean1 = reactive(input$mean1),
                                       mean2 = reactive(input$mean2),
                                       sd1 = reactive(input$sd1),
                                       sd2 = reactive(input$sd2),
                                       n_sample1 = reactive(input$n_mean1),
                                       n_sample2 = reactive(input$n_mean2),
                                       sd_Type = reactive(input$sd_Type2),
                                       conf_int = reactive(input$conf_int),
                                       calculate = reactive(input$calculate2),
                                       properTest = reactive(input$properTest),
                                       sd_assumption = reactive(input$sd_assumption)
                 )
                 
                 output$dynamic_plot2 <- renderUI(distributionPlot_ui("meanTest2"))
                 output$step_result2 <- renderUI(stepResult_ui("meanTest2"))
               },
               
               "Proportion Test" = {
                 twoPopPropTest_server("propTest2",
                                       p_diff = reactive(input$p_diff),
                                       n_interest1 = reactive(input$n_interest1),
                                       n_interest2 = reactive(input$n_interest2),
                                       n_sample1 = reactive(input$n_prop1),
                                       n_sample2 = reactive(input$n_prop2),
                                       conf_int = reactive(input$conf_int),
                                       calculate = reactive(input$calculate2),
                                       properTest = reactive(input$properTest),
                                       assumption = reactive(input$prop_assumption)
                 )
                 
                 output$dynamic_plot2 <- renderUI(distributionPlot_ui("propTest2"))
                 output$step_result2 <- renderUI(stepResult_ui("propTest2"))
               },
               
               "Variance Test" = {
                 twoPopVarTest_server("varTest2",
                                      var_sample1 = reactive(input$var_sample1),
                                      var_sample2 = reactive(input$var_sample2),
                                      n_sample1 = reactive(input$n_var1),
                                      n_sample2 = reactive(input$n_var2),
                                      conf_int = reactive(input$conf_int),
                                      calculate = reactive(input$calculate2),
                                      properTest = reactive(input$properTest)
                 )
                 
                 output$dynamic_plot2 <- renderUI(distributionPlot_ui("varTest2"))
                 output$step_result2 <- renderUI(stepResult_ui("varTest2"))
               },
        )
      }
    }) 
  })
  
  observeEvent(input$home, {
    output$home_nav_ui <- renderUI(render_homeNav_ui("homeNav"))
    
    output$home_tab_ui <- renderUI(render_homeTab_ui(id = "one_pop_panel", type = "one"))
    
    observeEvent(input$one_pop_test, {
      updateTabsetPanel(inputId= "popTest1", selected = input$one_pop_test)
    })
    
    output$sidebarInput_ui <- renderUI(
      sidebarInput_ui(id= "one_pop", type = "One Population")
    )
    
    observeEvent(input$one_pop_panel, {
      output$home_tab_ui <- renderUI(render_homeTab_ui(id = "one_pop_panel", type = "one"))
      
      observeEvent(input$one_pop_test, {
        updateTabsetPanel(inputId= "popTest1", selected = input$one_pop_test)
      })
      
      observeEvent(input$one_pop_panel, {
        output$sidebarInput_ui <- renderUI(
          sidebarInput_ui(id= "one_pop", type = "One Population")
        )
      })
      
      # Calculation module
      observeEvent(input$calculate1, {
        if(!is.null(input$one_pop_test) && length(input$one_pop_test) == 1){
          switch(input$one_pop_test,
                 "Mean Test" = {
                   onePopMeanTest_server("meanTest",
                                         mu = reactive(input$mu),
                                         mean = reactive(input$mean),
                                         sd = reactive(input$sd),
                                         n_sample = reactive(input$n_mean),
                                         sd_Type = reactive(input$sd_Type1),
                                         conf_int = reactive(input$conf_int),
                                         calculate = reactive(input$calculate1),
                                         properTest = reactive(input$properTest)
                   )
                   
                   output$dynamic_plot1 <- renderUI(distributionPlot_ui("meanTest"))
                   output$step_result1 <- renderUI(stepResult_ui("meanTest"))
                 },
                 
                 "Proportion Test" = {
                   onePopPropTest_server("propTest",
                                         p_pop = reactive(input$p_pop),
                                         p_sample = reactive(input$p_sample),
                                         n_sample = reactive(input$n_prop),
                                         conf_int = reactive(input$conf_int),
                                         calculate = reactive(input$calculate1),
                                         properTest = reactive(input$properTest)
                   )
                   
                   output$dynamic_plot1 <- renderUI(distributionPlot_ui("propTest"))
                   output$step_result1 <- renderUI(stepResult_ui("propTest"))
                 },
                 
                 "Variance Test" = {
                   onePopVarTest_server("varTest",
                                        var_pop = reactive(input$var_pop),
                                        var_sample = reactive(input$var_sample),
                                        n_sample = reactive(input$n_var),
                                        conf_int = reactive(input$conf_int),
                                        calculate = reactive(input$calculate1),
                                        properTest = reactive(input$properTest)
                   )
                   
                   output$dynamic_plot1 <- renderUI(distributionPlot_ui("varTest"))
                   output$step_result1 <- renderUI(stepResult_ui("varTest"))
                 },
          )
        }
      })
    })
    
    observeEvent(input$two_pop_panel, {
      output$home_tab_ui <- renderUI(render_homeTab_ui(id = "two_pop_panel", type = "two"))
      
      observeEvent(input$two_pop_test, {
        updateTabsetPanel(inputId = "popTest2", selected = input$two_pop_test)
      })
      
      observeEvent(input$two_pop_panel, {
        output$sidebarInput_ui <- renderUI(
          sidebarInput_ui(id = "two_pop", type = "Two Population")
        )
      })
      
      # for two population
      observeEvent(input$prop_assumption, {
        if(input$prop_assumption == "Equals") {
          output$prop_params <- renderUI({
            NULL
          })
        } else {
          output$prop_params <- renderUI({
            tagList(
              numericInput("p_diff", "Population Proportion Difference", value = 0.08),
            )
          })
        }
      })
      
      observeEvent(input$sd_Type2, {
        if(input$sd_Type2 == "Sample") {
          output$sd_assumption <- renderUI({
            selectInput("sd_assumption", "Assumption for Population SD", choices = param_assumption)
          })
        } else {
          output$sd_assumption <- renderUI({NULL})
        }
      })
      
      observeEvent(input$calculate2, {
        if(!is.null(input$two_pop_test) && length(input$two_pop_test) == 1){
          switch(input$two_pop_test,
                 "Mean Test" = {
                   twoPopMeanTest_server("meanTest2",
                                         mu_dif = reactive(input$mu_dif),
                                         mean1 = reactive(input$mean1),
                                         mean2 = reactive(input$mean2),
                                         sd1 = reactive(input$sd1),
                                         sd2 = reactive(input$sd2),
                                         n_sample1 = reactive(input$n_mean1),
                                         n_sample2 = reactive(input$n_mean2),
                                         sd_Type = reactive(input$sd_Type2),
                                         conf_int = reactive(input$conf_int),
                                         calculate = reactive(input$calculate2),
                                         properTest = reactive(input$properTest),
                                         sd_assumption = reactive(input$sd_assumption)
                   )
                   
                   output$dynamic_plot2 <- renderUI(distributionPlot_ui("meanTest2"))
                   output$step_result2 <- renderUI(stepResult_ui("meanTest2"))
                 },
                 
                 "Proportion Test" = {
                   twoPopPropTest_server("propTest2",
                                         p_diff = reactive(input$p_diff),
                                         n_interest1 = reactive(input$n_interest1),
                                         n_interest2 = reactive(input$n_interest2),
                                         n_sample1 = reactive(input$n_prop1),
                                         n_sample2 = reactive(input$n_prop2),
                                         conf_int = reactive(input$conf_int),
                                         calculate = reactive(input$calculate2),
                                         properTest = reactive(input$properTest),
                                         assumption = reactive(input$prop_assumption)
                   )
                   
                   output$dynamic_plot2 <- renderUI(distributionPlot_ui("propTest2"))
                   output$step_result2 <- renderUI(stepResult_ui("propTest2"))
                 },
                 
                 "Variance Test" = {
                   twoPopVarTest_server("varTest2",
                                        var_sample1 = reactive(input$var_sample1),
                                        var_sample2 = reactive(input$var_sample2),
                                        n_sample1 = reactive(input$n_var1),
                                        n_sample2 = reactive(input$n_var2),
                                        conf_int = reactive(input$conf_int),
                                        calculate = reactive(input$calculate2),
                                        properTest = reactive(input$properTest)
                   )
                   
                   output$dynamic_plot2 <- renderUI(distributionPlot_ui("varTest2"))
                   output$step_result2 <- renderUI(stepResult_ui("varTest2"))
                 },
          )
        }
      }) 
    })
  })
}

shinyApp(ui, server)