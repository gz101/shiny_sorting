#Sort Visualizer
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(dashboardthemes)

source("jaxmat.R")


stylesheet <-
  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css"))


ui <- dashboardPage(
  dashboardHeader(title="Learning to Sort"),
  dashboardSidebar(
    width=150,
    h5(id="choicetext","Choose sorting algorithm to learn:"),
    actionButton("selection", "Selection Sort"),
    actionButton("insertion", "Insertion Sort"),
    actionButton("shell", "Shell Sort"),
    actionButton("merge", "Merge Sort")
  ),
  dashboardBody(fluidRow(
    useShinyjs(),
    stylesheet,
    shinyDashboardThemes(
      theme="onenote"
    ),
    column(
      width=6,
      tags$div(
        class="column",
        uiOutput("header"),
        p("Learn a bit about the algorithm before seeing it in action."),
        actionBttn("learnbtn", "Background Information"),
        p(""),
        actionBttn("complexitybtn", "Algorithm Complexity"),
        h3("Step 1. Select number of elements to sort"),
        sliderInput(
          "genN",
          "Number of elements to sort, N",
          5,
          20,
          10
        ),
        h3("Step 2. Generate N Random Elements"),
        actionButton("btnrand", "Generate"),
        h3("Step 3a. Step-by-step Sort"),
        h5("Click 'Next Step' for manual step through, or 'Automatic' for automated steps to the end."),
        actionButton("btnnext", "Next Step"),
        actionButton("btnauto", "Automatic Steps"),
        htmlOutput("results"),
        h3("Step 3b. Final Sorted Form"),
        actionButton("btnsort", "Skip to Final"),
        br(), hr(),
        actionBttn("resetall", "Reset Everything")
      )
    ),
    column(
      width=6,
      tags$div(
        class="column",
        uiOutput("space"),
        plotOutput("startplot", width="100%", height="250px"),
        plotOutput("stepsort", width="100%", height="250px"),
        plotOutput("sortedplot", width="100%", height="250px")
      )
    )
  ))
)


server <- function(input, output, session) {
  #Global variables accessible to server()
  state <- "selection"
  output$header <- renderUI(h1("Selection Sort"))
  output$space <- renderUI(h1("Output Space"))
  N <- 10
  x <- numeric(0)
  stepx <- x
  stepcount <- 1
  insertcount <- 1
  color <- numeric(0)
  result.list <- ""
  generated <- FALSE
  jcount <- 2
  output$startplot <- renderPlot("")
  output$stepsort <- renderPlot("")
  output$sortedplot <- renderPlot("")
  h.shell <- 1
  i.shell <- h.shell
  j.shell <- i.shell + 1
  arrsize <- N
  merge.step <- 1
  ready.to.merge <- FALSE
  merge.sorted <- FALSE
  rv <- reactiveValues(loop=0)
  
  observeEvent(input$btnauto, {
    if (generated == FALSE) return()

    if (state == "selection") {
      if (stepcount == N) return()
      rv$loop <<- 1
    } else if (state == "insertion") {
      if (insertcount == N) return()
      rv$loop <<- 1
    } else if (state == "shell") {
      if (h.shell < 1) return()
      rv$loop <<- 1
    } else if (state == "merge") {
      rv$loop <<- 1
    }
  })
  observeEvent(rv$loop, {
    if (state == "selection" && rv$loop > 0 && stepcount < N) {
      Selection.stepsort()
      delay(1000, rv$loop <<- rv$loop + 1)
    } else if (state == "insertion" && rv$loop > 0 && jcount < N) {
      Insertion.stepsort()
      delay(1000, rv$loop <<- rv$loop + 1)
    } else if (state == "shell" && h.shell >= 1 && rv$loop > 0) {
      Shell.stepsort()
      delay(1000, rv$loop <<- rv$loop + 1)
    } else if (state == "merge" && merge.sorted == FALSE && rv$loop > 0) {
      Merge.stepsort()
      delay(1000, rv$loop <<- rv$loop + 1)
    }
  })
  
  #Functions
  #Complexity modal
  observeEvent(input$complexitybtn, {
    if (state == "selection") {
      showModal(
        modalDialog(
          title="Selection Sort",
          id="complexselectmodal",
          h4("Time Complexity of Selection Sort"),
          withTags(table(
            tr(th("Best"), th("Average"), th("Worst")),
            tr(td(jaxD("`Omega(n^2)")), td(jaxD("`Theta(n^2)")), td(jaxD("O(n^2)")))
          )),
          h4("Space Complexity of Selection Sort"),
          withTags(table(
            tr(th("Worst")),
            tr(td(jaxD("O(1)")))
          )),
          tags$em("Source: "),
          tags$a(href="https://www.bigocheatsheet.com/", "Big O Cheatsheet")
        )
      )
    } else if (state == "insertion") {
      showModal(
        modalDialog(
          title="Insertion Sort",
          id="complexinsertmodal",
          h4("Time Complexity of Insertion Sort"),
          withTags(table(
            tr(th("Best"), th("Average"), th("Worst")),
            tr(td(jaxD("`Omega(n)")), td(jaxD("`Theta(n^2)")), td(jaxD("O(n^2)")))
          )),
          h4("Space Complexity of Insertion Sort"),
          withTags(table(
            tr(th("Worst")),
            tr(td(jaxD("O(1)")))
          )),
          tags$em("Source: "),
          tags$a(href="https://www.bigocheatsheet.com/", "Big O Cheatsheet")
        )
      )
    } else if (state == "shell") {
      showModal(
        modalDialog(
          title="Shell Sort",
          id="complexshellmodal",
          h4("Time Complexity of Shell Sort"),
          p("The time complexity of Shell Sort is largely dependent on the selection of the step size."),
          withTags(table(
            tr(th("General Term", jaxI("(k `geqslant 1)")), th("Worst"), th("Author & Year of Publication")),
            tr(td(jaxD("`Bigl`lfloor`frac{N}{2^k}`Bigl`rfloor")), td(jaxD("`Theta(n^2)")), td("Shell, 1959")),
            tr(td(jaxD("2 `Bigl`lfloor`frac{N}{2^{k+1}}`Bigl`rfloor + 1")), td(jaxD("`Theta(n^{`frac{3}{2}})")), td("Frank & Lazarus, 1960")),
            tr(td(jaxD("2^k-1")), td(jaxD("`Theta(n^{`frac{3}{2}})")), td("Hibbard, 1963")),
            tr(td(jaxD("2^k+1")), td(jaxD("`Theta(n^{`frac{3}{2}})")), td("Papernov & Stasevich, 1965")),
            tr(td("Successive numbers of the form", jaxI("2^p3^q")), td(jaxD("`Theta(n`log^2n)")), td("Pratt, 1971")),
            tr(td(jaxD("`prod_{I}a_q, where"), 
                  jaxD("a_0=3"), 
                  jaxD("a_q = min`Bigl`{n`in`mathbb{N}:n `geqslant(`frac{5}{2})^{q+1},"),
                  jaxD("`forall p:0 `leqslant p<q`Rightarrow gcd(a_p,n)=1`Bigl`}"),
                  jaxD("I = `Bigl`{0 `leqslant q < r `mid q`ne `frac{1}{2}(r^2+r)k`Bigl`}"),
                  jaxD("r=`Bigl`lfloor `sqrt{2k + `sqrt{2k}} `Bigl`rfloor")), 
               td(jaxD("O`big(n^{1+`sqrt{`frac{8`ln(5/2)}{`ln(n)}}}`big)")), 
               td("Incepri, Sedgewick & Knuth, 1985")),
            tr(td(jaxD("4^k+3 `times 2^k+1")), td(jaxD("O(n^{`frac{4}{3}})")), td("Sedgewick, 1986"))
          )),
          h4("Space Complexity of Shell Sort"),
          withTags(table(
            tr(th("Worst")),
            tr(td(jaxD("O(1)")))
          )),
          tags$em("Source: "),
          tags$a(href="https://en.wikipedia.org/wiki/Shellsort#Gap_sequences", "Wikipedia")
        )
      )
    } else if (state == "merge") {
      showModal(
        modalDialog(
          title="Merge Sort (Bottom Up)",
          id="complexmergemodal",
          h4("Time Complexity of Merge Sort"),
          withTags(table(
            tr(th("Best"), th("Average"), th("Worst")),
            tr(td(jaxD("`Omega(n`log(n))")), td(jaxD("`Theta(n`log(n))")), td(jaxD("O(n`log(n))")))
          )),
          h4("Space Complexity of Merge Sort"),
          withTags(table(
            tr(th("Worst")),
            tr(td(jaxD("O(n)")))
          )),
          tags$em("Source: "),
          tags$a(href="https://www.bigocheatsheet.com/", "Big O Cheatsheet")
        )
      )
    }
  })
  
  #Background info modal
  observeEvent(input$learnbtn, {
    if (state == "selection") {
      showModal(
        modalDialog(
          title="Selection Sort",
          id="learnselectmodal",
          h4("What is Selection Sort?"),
          p("The selection sort algorithm sorts an array by repeatedly finding 
            the minimum element (considering ascending order) from unsorted part 
            and putting it at the beginning. The algorithm maintains two 
            subarrays in a given array."
          ),
          withTags(ol(
            li("The subarray which is already sorted."),
            li("Remaining subarray which is unsorted.")
          )),
          p("In every iteration of selection sort, the minimum element 
            (considering ascending order) from the unsorted subarray is picked 
            and moved to the sorted subarray."
          ),
          tags$em("Source: "),
          tags$a(href="https://www.geeksforgeeks.org/selection-sort/", "GeeksforGeeks"),
          h4("Selection Sort Visualizer"),
          tags$iframe(
            width="100%",
            height="315px",
            src="https://www.youtube.com/embed/92BfuxHn2XE",
            frameborder="0",
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allow="encrypted-media"
          ),
          tags$em("Source: "),
          tags$a(href="https://www.youtube.com/watch?v=92BfuxHn2XE&ab_channel=TimoBingmann", "Timo Bingmann - Youtube")
        )
      )
    } else if (state == "insertion") {
      showModal(
        modalDialog(
          title="Insertion Sort",
          id="learninsertmodal",
          h4("What is Insertion Sort?"),
          p("Insertion sort is a simple sorting algorithm that works similar to 
            the way you sort playing cards in your hands. The array is virtually 
            split into a sorted and an unsorted part. Values from the unsorted 
            part are picked and placed at the correct position in the sorted part."
          ),
          tags$strong("Algorithm:"),
          p("To sort an array of size N in ascending order:"),
          withTags(ol(
            li("Iterate from arr[1] to arr[N] over the array."),
            li("Compare the current element (key) to its predecessor."),
            li("If the key element is smaller than its predecessor, compare it 
               to the elements before."),
            li("Move the greater elements one position up to make space for the 
               swapped element.")
          )),
          tags$em("Source: "),
          tags$a(href="https://www.geeksforgeeks.org/insertion-sort/", "GeeksforGeeks"),
          h4("Insertion Sort Visualizer"),
          tags$iframe(
            width="100%",
            height="315px",
            src="https://www.youtube.com/embed/8oJS1BMKE64",
            frameborder="0",
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allow="encrypted-media"
          ),
          tags$em("Source: "),
          tags$a(href="https://www.youtube.com/watch?v=8oJS1BMKE64&ab_channel=TimoBingmann", "Timo Bingmann - Youtube")
        )
      )
    } else if (state == "shell") {
      showModal(
        modalDialog(
          title="Shell Sort",
          id="learnshellmodal",
          h4("What is Shell Sort?"),
          p("Shell sort is mainly a variation of Insertion Sort. In insertion sort, 
            we move elements only one position ahead. When an element has to be 
            moved far ahead, many movements are involved. The idea of shell sort 
            is to allow exchange of far items. In shell sort, we make the array 
            h-sorted for a large value of h. We keep reducing the value of h 
            until it becomes 1. An array is said to be h-sorted if all sublists 
            of every h'th element is sorted."
          ),
          tags$strong("Algorithm:"),
          p("To sort an array of size N in ascending order:"),
          withTags(ol(
            li("Initialize h (this will determine the time complexity and there are a few choices to choose from: ", tags$a(href="https://en.wikipedia.org/wiki/Shellsort#Gap_sequences", "see here"), ")."),
            li("Compare the current element (key) to its predecessor (current - h) indexes away."),
            li("If the key element is smaller, exchange the items."),
            li("Repeat until the end of the array, then reduce h by a factor that you used to initialize h."),
            li("Repeat steps 2 to 4 until h is 1."),
            li("The final iteration will essentially be an insertion sort.")
          )),
          tags$em("Source: "),
          tags$a(href="https://www.geeksforgeeks.org/shellsort/", "GeeksforGeeks"),
          h4("Shell Sort Visualizer"),
          tags$iframe(
            width="100%",
            height="315px",
            src="https://www.youtube.com/embed/n4sk-SzGvZA",
            frameborder="0",
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allow="encrypted-media"
          ),
          tags$em("Source: "),
          tags$a(href="https://www.youtube.com/watch?v=n4sk-SzGvZA&ab_channel=TimoBingmann", "Timo Bingmann - Youtube")
        )
      )
    } else if (state == "merge") {
      showModal(
        modalDialog(
          title="Merge Sort (Bottom Up)",
          id="learnmergemodal",
          h4("What is Merge Sort?"),
          p("Merge Sort is a Divide and Conquer algorithm. It divides the input 
            array into two halves, calls itself for the two halves, and then merges 
            the two sorted halves. The merge() function is used for merging two 
            halves. The merge(arr, lo, mid, hi) is a key process that assumes that 
            arr[lo..mid] and arr[mid+1..hi] are sorted and merges the two sorted 
            sub-arrays into one."
          ),
          p("Bottom Up Merge Sort is a variation on the classic Merge Sort algorithm 
            which organizes the merges such that we do all the merges of tiny subarrays on 
            one pass, then do a second pass to merge those subarrays in pairs and so forth 
            continuing until we merge a subarray containing the entire array. It has the
            same upperbound runtime as the classic Merge Sort algorithm."
          ),
          tags$strong("Algorithm:"),
          p("To sort an array of size N:"),
          withTags(ol(
            li("If size of array > 1"),
            li("Keep dividing the array until a subarray size of 1 is obtained."),
            li("Starting with length=1 and doubling length on each pass, do length-by-length merges on each pass"),
            li("The final pass will occur when length is the size of the whole array.")
          )),
          tags$em("Source: "),
          tags$a(href="https://algs4.cs.princeton.edu/22mergesort/", "Algorithms, 4th Edition"),
          h4("Merge Sort Visualizer"),
          tags$iframe(
            width="100%",
            height="315px",
            src="https://www.youtube.com/embed/NMdIhtfflMU",
            frameborder="0",
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allow="encrypted-media"
          ),
          tags$em("Source: "),
          tags$a(href="https://www.youtube.com/watch?v=NMdIhtfflMU&ab_channel=Algorithmsexplained", "Algorithms Explained - Youtube")
        )
      )
    }
  })
  
  #Hard reset
  observeEvent(input$resetall, {
    showModal(
      modalDialog(
        title="WARNING: All current outputs will be reset!",
        h3("You will reset all outputs and internal states if you click CONFIRM."),
        footer=tagList(
          actionButton("confirm", "CONFIRM"),
          modalButton("Return Back")
        )
      )
    )
  })
  
  observeEvent(input$confirm, {
    restart()
    generated <<- FALSE
    removeModal()
  })
  
  #Page header
  observeEvent(input$selection, {
    if (state != "selection") {
      state <<- "selection"
      restart()
      generated <<- FALSE
      output$header <- renderUI(h1("Selection Sort"))
      output$space <- renderUI(h1("Output Space"))
    }
  })
  observeEvent(input$insertion, {
    if (state != "insertion") {
      state <<- "insertion"
      restart()
      generated <<- FALSE
      output$header <<- renderUI(h1("Insertion Sort"))
      output$space <- renderUI(h1("Output Space"))
    }
  })
  observeEvent(input$shell, {
    if (state != "shell") {
      state <<- "shell"
      restart()
      generated <<- FALSE
      output$header <<- renderUI(h1("Shell Sort"))
      output$space <- renderUI(h1("Output Space"))
    }
  })
  observeEvent(input$merge, {
    if (state != "merge") {
      state <<- "merge"
      restart()
      generated <<- FALSE
      output$header <<- renderUI(h1("Merge Sort (Bottom Up)"))
      output$space <- renderUI(h1("Output Space"))
    }
  })
  
  #Generate random vector
  observeEvent(input$btnrand, {
    restart()
    printPlot1()
  })
  
  #Reset all variables to blank
  restart <- function() {
    N <<- input$genN
    x <<- sample(1:input$genN, input$genN, replace=F)
    stepx <<- x
    stepcount <<- 1
    insertcount <<- 1
    color <<- numeric(0)
    result.list <<- ""
    generated <<- TRUE
    jcount <<- 2
    output$startplot <- renderPlot("")
    output$stepsort <- renderPlot("")
    output$sortedplot <- renderPlot("")
    printSteps(-1, -1)
    h.shell <<- 1
    i.shell <<- h.shell
    j.shell <<- i.shell + 1
    arrsize <<- N
    merge.step <<- 1
    ready.to.merge <<- FALSE
    merge.sorted <<- FALSE
    rv$loop <<- -50
  }
  
  #Sort to final step button
  observeEvent(input$btnsort, {
    if (state == "selection") {
      Selection.sort()
    } else if (state == "insertion") {
      Insertion.sort()
    } else if (state == "shell") {
      Shell.sort()
    } else if (state == "merge") {
      Merge.sort(x)
    }
  })
  
  #Sort step by step
  observeEvent(input$btnnext, {
    if (state == "selection") {
      Selection.stepsort()
    } else if (state == "insertion") {
      Insertion.stepsort()
    } else if (state == "shell") {
      Shell.stepsort()
    } else if (state == "merge") {
      Merge.stepsort()
    }
  })
  
  #Sort functions
  #Full insertion sort
  Insertion.sort <- function() {
    if (generated == FALSE) {
      return()
    }
    for (i in 2:N) {
      j <- i
      while (j > 1) {
        if (x[j] < x[j - 1]) {
          t <- x[j - 1]
          x[j - 1] <<- x[j]
          x[j] <<- t
        }
        j <- j - 1
      }
    }
    printPlot2()
  }
  
  #Full selection sort
  Selection.sort <- function() {
    if (generated == FALSE) {
      return()
    }
    for (i in 1:(N - 1)) {
      smallest <- i
      k <- i + 1
      for (j in k:N) {
        if (x[j] < x[smallest]) {
          smallest <- j
        }
      }
      t <- x[i]
      x[i] <<- x[smallest]
      x[smallest] <<- t
    }
    printPlot2()
  }
  
  #Full Shell sort
  Shell.sort <- function() {
    if (generated == FALSE) {
      return()
    }
    h <- 1
    while (h < N/3) h <- h * 3 + 1
    
    while (h >= 1) {
      for (i in h:(N-1)) {
        j <- i + 1
        while (j > h) {
          if (x[j] < x[j-h]) {
            t <- x[j - 1]
            x[j - 1] <<- x[j]
            x[j] <<- t
          }
          j <- j - h
        }
      }
      h <- round(h/3)
    }
    printPlot2()
  }
  
  #Full Merge sort
  private.merge <- function(a, b) {
    r <- numeric(length(a) + length(b))
    ai<-1; bi<-1; j<-1;
    for(j in 1:length(r)) {
      if((ai <= length(a) && a[ai] < b[bi]) || bi > length(b)) {
        r[j] <- a[ai]
        ai <- ai+1
      } else {
        r[j] <- b[bi]
        bi <- bi+1          
      }
    }
    r
  }
  Merge.sort <- function(A) {
    if (generated==FALSE) return()
    
    if(length(A)>1) {
      q <- ceiling(length(A) / 2)
      a <- Merge.sort(A[1:q])
      b <- Merge.sort(A[(q+1):length(A)])
      x <<- private.merge(a, b)
    } else {
      printPlot2()
      A
    }
  }
  
  #Step by step merge sort
  Merge.stepsort <- function() {
    if (generated == FALSE) return()
    if (merge.sorted == TRUE) return()
    
    #Consider left half
    if (!ready.to.merge && arrsize > 1) {
      arrsize <<- ceiling(arrsize / 2)
      resolve.color.merge(1, arrsize)
      printPlot3()
      printSteps.merge(1, arrsize, FALSE, arrsize)
      stepcount <<- stepcount + 1
      return()
    }
    if (arrsize == 1) {
      ready.to.merge <<- TRUE
      arrsize <<- arrsize * 2
      merge.step <<- 1
      resolve.color.merge(1, arrsize)
      printPlot3()
      line.out <- paste0(stepcount, ". Sub arrays of size 1. Ready to merge.")
      result.list <<- paste(result.list, line.out, sep="<br/>")
      output$results <- renderUI(HTML(result.list))
      stepcount <<- stepcount + 1
      return()
    }
    #Merge size 2
    if (ready.to.merge && arrsize == 2 && merge.step <= N-1) {
      v <- stepx[merge.step:(merge.step+1)]
      v <- sort(v)
      stepx[merge.step] <<- v[1]
      stepx[merge.step+1] <<- v[2]
      resolve.color.merge(merge.step, merge.step + 1)
      printPlot3()
      
      printSteps.merge(merge.step, merge.step+1, TRUE, arrsize)
      stepcount <<- stepcount + 1
      merge.step <<- merge.step + arrsize

      if (merge.step > N-1) {
        arrsize <<- arrsize * 2
        merge.step <<- 1
      }
      return()
    }
    #Merge size 4
    if (ready.to.merge && arrsize == 4 && merge.step <= N-3) {
      v <- stepx[merge.step:(merge.step+3)]
      v <- sort(v)
      for (i in merge.step:(merge.step+3)) {
        stepx[i] <<- v[i - merge.step + 1]
      }
      resolve.color.merge(merge.step, merge.step + 3)
      printPlot3()
      
      printSteps.merge(merge.step, ceiling(merge.step+(4/2)), TRUE, arrsize)
      stepcount <<- stepcount + 1
      merge.step <<- merge.step + arrsize
      
      if (merge.step > N-3) {
        arrsize <<- arrsize * 2
        merge.step <<- 1
      }
      return()
    }
    #Merge size 8
    if (N > 8 && ready.to.merge && arrsize == 8 && merge.step <= N-7) {
      v <- stepx[merge.step:(merge.step+7)]
      v <- sort(v)
      for (i in merge.step:(merge.step+7)) {
        stepx[i] <<- v[i - merge.step + 1]
      }
      resolve.color.merge(merge.step, merge.step + 7)
      printPlot3()
      
      printSteps.merge(merge.step, ceiling(merge.step+(8/2)), TRUE, arrsize)
      stepcount <<- stepcount + 1
      
      merge.step <<- merge.step + arrsize
      
      if (merge.step > N-7) {
        arrsize <<- arrsize * 2
        merge.step <<- 1
      }
      return()
    }
    #Merge size 16
    if (N > 16 && ready.to.merge && arrsize == 16 && merge.step <= N-15) {
      v <- stepx[merge.step:(merge.step+15)]
      v <- sort(v)
      for (i in merge.step:(merge.step+15)) {
        stepx[i] <<- v[i - merge.step + 1]
      }
      resolve.color.merge(merge.step, merge.step + 15)
      printPlot3()
      
      printSteps.merge(merge.step, ceiling(merge.step+(16/2)), TRUE, arrsize)
      stepcount <<- stepcount + 1
      merge.step <<- merge.step + arrsize
      
      if (merge.step > N-15) {
        arrsize <<- arrsize * 2
        merge.step <<- 1
      }
      return()
    }
    #Final merge
    if (arrsize > (N + 1) / 2) {
      stepx <<- sort(stepx)
      resolve.color.merge(1, N)
      printPlot3()
      arrsize <<- arrsize * 2
      printSteps.merge(merge.step, ceiling((N+1)/2), TRUE, N)
      stepcount <<- stepcount + 1
      merge.sorted <<- TRUE
      return()
    }
  }
  
  #Step by step shell sort
  Shell.stepsort <- function() {
    if (generated == FALSE) return()
    if (h.shell < 1) return()
    if (stepcount == 1) {
      while (h.shell < N/3) h.shell <<- h.shell * 3 + 1
      i.shell <<- h.shell
      j.shell <<- i.shell + 1
    }

    if (j.shell <= h.shell) {
      i.shell <<- i.shell + 1
      j.shell <<- i.shell + 1
    }
    if (i.shell == N) {
      h.shell <<- round(h.shell / 3)
      i.shell <<- h.shell
      j.shell <<- i.shell + 1
    }
    
    if (stepx[j.shell] < stepx[j.shell - h.shell]) {
      t <- stepx[j.shell - h.shell]
      stepx[j.shell - h.shell] <<- stepx[j.shell]
      stepx[j.shell] <<- t
      printSteps.shell(j.shell, j.shell - h.shell, h.shell)
    } else {
      printSteps.shell(j.shell, j.shell, h.shell)
    }
    stepcount <<- stepcount + 1
    resolve.color(j.shell, j.shell - h.shell)
    printPlot3()
    j.shell <<- j.shell - h.shell
  }
  
  #Step by step insertion sort
  Insertion.stepsort <- function() {
    if (generated == FALSE) {
      return()
    }
    if (insertcount == N) {
      return()
    }
    if (jcount == 1) {
      insertcount <<- insertcount + 1
      jcount <<- insertcount + 1
    }
    if (jcount > N) {
      return()
    }
    if (stepx[jcount] < stepx[jcount - 1]) {
      t <- stepx[jcount - 1]
      stepx[jcount - 1] <<- stepx[jcount]
      stepx[jcount] <<- t
      printSteps(jcount - 1, jcount)
    } else {
      printSteps(jcount, jcount)
    }
    stepcount <<- stepcount + 1
    resolve.color(jcount, jcount - 1)
    printPlot3()
    jcount <<- jcount - 1
  }
  
  #Step by step selection sort
  Selection.stepsort <- function() {
    if (generated == FALSE) {
      return()
    }
    if (stepcount == N) {
      return()
    }
    smallest <- stepcount
    k <- smallest + 1
    for (j in k:N) {
      if (stepx[j] < stepx[smallest]) {
        smallest <- j
      }
    }
    resolve.color(stepcount, smallest)
    printSteps(stepcount, smallest)
    t <- stepx[stepcount]
    stepx[stepcount] <<- stepx[smallest]
    stepx[smallest] <<- t
    stepcount <<- stepcount + 1
    printPlot3()
  }
  
  #Utility functions
  #Show step by step text
  printSteps <- function(idx1, idx2) {
    if (idx1 == -1 || idx2 == -1) {
      output$results <- renderUI("")
    } else {
      if (idx1 == idx2) {
        line.out <- paste0(stepcount, ". Index ", idx2, " stays the same.")
      } else {
        line.out <- paste0(stepcount, ". Swap index ", idx1, " with index ", idx2, ".")
      }
      result.list <<- paste(result.list, line.out, sep="<br/>")
      output$results <- renderUI(HTML(result.list))
    }
  }
  #printSteps for merge
  printSteps.merge <- function(idx1, idx2, merge=FALSE, size=-1) {
    if (idx1 == -1 || idx2 == -1) {
      output$results <- renderUI("")
    }
    if (merge) {
      line.out <- paste0(stepcount, 
                         ". Merge left array starting at: ", 
                         idx1, 
                         " with right array starting at: ", 
                         idx2, " (resultant array size:", size, ").")
    } else {
      line.out <- paste0(stepcount, ". Consider subarray of size: ", size, ".")
    }
    result.list <<- paste(result.list, line.out, sep="<br/>")
    output$results <- renderUI(HTML(result.list))
  }
  #printSteps for shell
  printSteps.shell <- function(idx1, idx2, h.step) {
    if (idx1 == -1 || idx2 == -1) {
      output$results <- renderUI("")
    } else {
      if (idx1 == idx2) {
        line.out <- paste0(stepcount, ". Index ", idx2, " stays the same. H-step is:", h.step, ".")
      } else {
        line.out <- paste0(stepcount, ". Swap index ", idx1, " with index ", idx2, ". H-step is:", h.step, ".")
      }
      result.list <<- paste(result.list, line.out, sep="<br/>")
      output$results <- renderUI(HTML(result.list))
    }
  }
  
  #Determine color of bars
  resolve.color <- function(idx1, idx2) {
    for (i in 1:N) {
      if (i == idx1 || i == idx2) {
        color[i] <<- "orange"
      } else {
        color[i] <<- "grey"
      }
    }
  }
  
  #Determine color of bars for merge sort
  resolve.color.merge <- function(idx1, idx2) {
    for (i in 1:N) {
      if (i %in% idx1:idx2) {
        color[i] <<- "orange"
      } else {
        color[i] <<- "grey"
      }
    }
  }
  
  #Initial plot
  printPlot1 <- function() {
    output$startplot <- renderPlot({
      barplot(
        height=x,
        width=0.7,
        names.arg=c(1:N),
        main="Starting Order",
        xlab="Element Position",
        ylab="Sort by Values"
      )
    })
  }
  
  #Final plot
  printPlot2 <- function() {
    output$sortedplot <- renderPlot({
      barplot(
        height=x,
        width=0.7,
        col=rep("green", N),
        names.arg=c(1:N),
        main="Final Sorted Order",
        xlab="Element Position",
        ylab="Sort by Values"
      )
    })
  }
  
  #Step by step plot
  printPlot3 <- function() {
    output$stepsort <- renderPlot({
      barplot(
        height=stepx,
        width=0.7,
        col=color,
        names.arg=c(1:N),
        main="Step-sorted Order",
        xlab="Element Position",
        ylab="Sort by Values"
      )
    })
  }
}


#Run the application
shinyApp(ui=ui, server=server)