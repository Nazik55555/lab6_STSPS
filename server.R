library(shiny)

shinyServer(
  
  function(input, output) {
    vuz <- function(a11, a12, a13, a21, a22, a23, a31, a32, a33) 
    {
      a11*a22*a33 + a21*a32*a13 + a12*a23*a31 - a31*a22*a13 - a12*a21*a33-a23*a32*a11
    }
    output$a = renderPrint(matrix(c(input$a11, input$a21, input$a31, input$a41, input$a12, input$a22, input$a32, input$a42, input$a13, input$a23, input$a33, input$a43, input$a14, input$a24, input$a34, input$a44), nrow = 4, ncol = 4))
    output$b1 = renderPrint(matrix(c(input$a22, input$a32, input$a42, input$a23, input$a33, input$a43, input$a24, input$a34, input$a44), nrow = 3, ncol = 3))
    output$b2 = renderPrint(matrix(c(input$a21, input$a31, input$a41, input$a23, input$a33, input$a43, input$a24, input$a34, input$a44), nrow = 3, ncol = 3))
    output$b3 = renderPrint(matrix(c(input$a21, input$a31, input$a41, input$a22, input$a32, input$a42, input$a24, input$a34, input$a44), nrow = 3, ncol = 3))
    output$b4 = renderPrint(matrix(c(input$a21, input$a31, input$a41, input$a22, input$a32, input$a42, input$a23, input$a33, input$a43), nrow = 3, ncol = 3))
    output$c1 = renderPrint(vuz(input$a22, input$a23, input$a24, input$a32, input$a33, input$a34, input$a42, input$a43, input$a44))
    output$c2 = renderPrint(vuz(input$a21, input$a23, input$a24, input$a31, input$a33, input$a34, input$a41, input$a43, input$a44))
    output$c3 = renderPrint(vuz(input$a21, input$a22, input$a24, input$a31, input$a32, input$a34, input$a41, input$a42, input$a44))
    output$c4 = renderPrint(vuz(input$a21, input$a22, input$a23, input$a31, input$a32, input$a33, input$a41, input$a42, input$a43))
    output$b = renderPrint(input$a11*vuz(input$a22, input$a23, input$a24, input$a32, input$a33, input$a34, input$a42, input$a43, input$a44) - input$a12*vuz(input$a21, input$a23, input$a24, input$a31, input$a33, input$a34, input$a41, input$a43, input$a44) + input$a13*vuz(input$a21, input$a22, input$a24, input$a31, input$a32, input$a34, input$a41, input$a42, input$a44) - input$a14*vuz(input$a21, input$a22, input$a23, input$a31, input$a32, input$a33, input$a41, input$a42, input$a43))
  }
)