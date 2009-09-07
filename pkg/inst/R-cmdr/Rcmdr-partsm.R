
MakeLoadpartsm <- function()
{
  library(partsm)
  addTclPath(system.file(package="partsm"))
  tclRequire("BWidget")
  tkmessageBox(message="Package 'partsm' is up and running.", icon="info", type="ok", title="Load 'partsm'")
}

Maketslist <- function()
{
  justDoIt(paste("cls <- ls(); ref <- rep(NA, length(cls))"))
  justDoIt(paste("for(i in 1:length(cls)) ref[i] <- class(get(cls[i]))"))
  justDoIt(paste("tslist <- cls[which(ref == \"ts\")]"))
}

MakeFpar.test <- function()
{
  #addTclPath(system.file(package="partsm"))
  #tclRequire("BWidget")

  #if (!checkActiveDataSet()) return()
  #if (!checkNumeric(n=1)) return()

  #title <- substr(type, 1, nchar(type)-5)
  #initializeDialog(title=title)
  initializeDialog(title="Fpar test")

  #tsdataBox <- variableListBox(top, Numeric(), title="Variable (pick one of class 'ts')")

  onOK <- function()
  {
    #wvar <- getSelection(tsdataBox)
    #wvar <- tclvalue(wvarVariable)
    ref <- tclvalue(tkcmd(tslistCBox,"getvalue"))
    tslist <- Maketslist()
    wvar <- tslist[as.numeric(ref)+1]

    #if(length(wvar) == 0 || length(wvar) > 1){
    #  errorCondition(recall=MakeFpar.test, message="You must select one variable.")
    #  return()
    #}
    #if(class(wvar != "ts")){
    #  errorCondition(recall=MakeFpar.test,
    #    message="The selected variable is not of class 'ts', i.e. a time series object.")
    #  return()
    #}

    #wvar <- paste(.activeDataSet, "$", wvar, sep="")

    dc1 <- tclvalue(sintercValue)
    dc2 <- tclvalue(strendValue)
    p <- tclvalue(orderVariable)

    if (GrabFocus()) tkgrab.release(top)
    tkdestroy(top)

    command1 <- paste("detcomp <- list(regular=c(0,0,0), seasonal=c(", dc1, ",", dc2, "), regvar=0)", sep="")
    justDoIt(command1)
    #if(type == "Fpar.test" || type == "LRurpar.test")
    #  command2 <- paste(type, "(wts=", wvar, ", detcomp=", detcomp, ", p=", p, ")", sep="")
    #if(type == "Fnextp.test")
    #  command2 <- paste(type, "(wts=", wvar, ", detcomp=", detcomp, ", p=", p, "type=\"PAR\")", sep="")
    command2 <- paste("Fpar.test(wts=", wvar, ", detcomp=detcomp, p=", p, ")", sep="")
    doItAndPrint(command2)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="Fpar.test")

  #checkBoxes(frame="dcFrame", boxes=c("sinterc", "strend"), initialValues=c("0","0"),
  #  labels=c("Seasonal intercepts", "Seasonal trends"))
  #, title="Deterministic components:") # No existe la opción title en check Boxes, sí en radioButtons.
  #tkgrid(tklabel(dcFrame, text="Deterministic components:", fg="blue"), sticky="nw")

  mainFrame <- tkframe(top)

  tslist <- Maketslist()
  wvarVariable <- tclVar("")
  tslistCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="7", values=tslist,
                  textvariable=wvarVariable)
  tkgrid(tklabel(mainFrame, text="Pick a time series:", fg="blue"), tslistCBox, sticky="w")
  tkgrid(mainFrame, sticky="w")
  #wvarEntry <- tkentry(mainFrame, width="7", textvariable=wvarVariable)

  #tkgrid(getFrame(tsdataBox), mainFrame, sticky="nw")
  #tkgrid(tklabel(mainFrame, text="Type the name of the time series: ", fg="blue"), wvarEntry, sticky="w")
  #tkgrid.configure(wvarEntry, sticky="e")

  orderVariable <- tclVar("1")
  ##orderEntry <- tkentry(mainFrame, width="2", textvariable=orderVariable)  # Para evitar BWidget.
  orderCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="3", values=c("1","2"),
                 textvariable=orderVariable)
  tkgrid(tklabel(mainFrame, text=""))
  tkgrid(tklabel(mainFrame, text="PAR model order = ", fg="blue"), orderCBox, sticky="w")

  tkgrid(mainFrame, sticky="w")

  ##tkgrid.configure(orderEntry, sticky="e")

  sinterc <- tkcheckbutton(mainFrame); sintercValue <- tclVar("1")
  strend <- tkcheckbutton(mainFrame); strendValue <- tclVar("0")
  tkgrid(tklabel(mainFrame, text="\nDeterministic components:", fg="blue"), sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal intercepts"), sinterc, sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal trends"), strend, sticky="w")

  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  dialogSuffix(rows=3, columns=2)  ##~ Ver.
}

MakeFnextp.test <- function()
{
  initializeDialog(title="Fnextp test")

  onOK <- function()
  {
    ref <- tclvalue(tkcmd(tslistCBox,"getvalue"))
    tslist <- Maketslist()
    wvar <- tslist[as.numeric(ref)+1]

    dc1 <- tclvalue(sintercValue)
    dc2 <- tclvalue(strendValue)
    p <- tclvalue(orderVariable)

    if (GrabFocus()) tkgrab.release(top)
    tkdestroy(top)

    command1 <- paste("detcomp <- list(regular=c(0,0,0), seasonal=c(", dc1, ",", dc2, "), regvar=0)", sep="")
    justDoIt(command1)
    command2 <- paste("Fnextp.test(wts=", wvar, ", detcomp=detcomp, p=", p, ", type=\"PAR\")", sep="")
    doItAndPrint(command2)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="Fnextp.test")

  mainFrame <- tkframe(top)

  tslist <- Maketslist()
  wvarVariable <- tclVar("")
  tslistCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="7", values=tslist,
                  textvariable=wvarVariable)
  tkgrid(tklabel(mainFrame, text="Pick a time series:", fg="blue"), tslistCBox, sticky="w")
  tkgrid(mainFrame, sticky="w")

  orderVariable <- tclVar("1")
  orderCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="3",
                        values=c("1","2", "3", "4"), textvariable=orderVariable)
  tkgrid(tklabel(mainFrame, text=""))
  tkgrid(tklabel(mainFrame, text="PAR model order = ", fg="blue"), orderCBox, sticky="w")

  tkgrid(mainFrame, sticky="w")

  sinterc <- tkcheckbutton(mainFrame); sintercValue <- tclVar("1")
  strend <- tkcheckbutton(mainFrame); strendValue <- tclVar("0")
  tkgrid(tklabel(mainFrame, text="\nDeterministic components:", fg="blue"), sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal intercepts"), sinterc, sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal trends"), strend, sticky="w")

  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  dialogSuffix(rows=3, columns=2)  ##~ Ver.
}

MakeLRurpar.test <- function()
{
  initializeDialog(title="LRurpar test")

  onOK <- function()
  {
    ref <- tclvalue(tkcmd(tslistCBox,"getvalue"))
    tslist <- Maketslist()
    wvar <- tslist[as.numeric(ref)+1]

    dc1 <- tclvalue(sintercValue)
    dc2 <- tclvalue(strendValue)
    p <- tclvalue(orderVariable)

    if (GrabFocus()) tkgrab.release(top)
    tkdestroy(top)

    command1 <- paste("detcomp <- list(regular=c(0,0,0), seasonal=c(", dc1, ",", dc2, "), regvar=0)", sep="")
    justDoIt(command1)
    command2 <- paste("LRurpar.test(wts=", wvar, ", detcomp=detcomp, p=", p, ")", sep="")
    doItAndPrint(command2)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="LRurpar.test")

  mainFrame <- tkframe(top)

  tslist <- Maketslist()
  wvarVariable <- tclVar("")
  tslistCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="7", values=tslist,
                  textvariable=wvarVariable)
  tkgrid(tklabel(mainFrame, text="Pick a time series:", fg="blue"), tslistCBox, sticky="w")
  tkgrid(mainFrame, sticky="w")

  orderVariable <- tclVar("1")
  orderCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="3", values=c("1","2"),
                 textvariable=orderVariable)
  tkgrid(tklabel(mainFrame, text=""))
  tkgrid(tklabel(mainFrame, text="PAR model order = ", fg="blue"), orderCBox, sticky="w")

  tkgrid(mainFrame, sticky="w")

  sinterc <- tkcheckbutton(mainFrame); sintercValue <- tclVar("1")
  strend <- tkcheckbutton(mainFrame); strendValue <- tclVar("0")
  tkgrid(tklabel(mainFrame, text="\nDeterministic components:", fg="blue"), sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal intercepts"), sinterc, sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal trends"), strend, sticky="w")

  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  dialogSuffix(rows=3, columns=2)  ##~ Ver.
}

Makefit.ar.par <- function()
{
  initializeDialog(title="Fit PAR model")

  onOK <- function()
  {
    ref <- tclvalue(tkcmd(tslistCBox,"getvalue"))
    tslist <- Maketslist()
    wvar <- tslist[as.numeric(ref)+1]

    dc1 <- tclvalue(sintercValue)
    dc2 <- tclvalue(strendValue)
    p <- tclvalue(orderVariable)

    if (GrabFocus()) tkgrab.release(top)
    tkdestroy(top)

    command1 <- paste("detcomp <- list(regular=c(0,0,0), seasonal=c(", dc1, ",", dc2, "), regvar=0)", sep="")
    justDoIt(command1)
    command2 <- paste("fit.ar.par(wts=", wvar, ", type=\"PAR\", detcomp=detcomp, p=", p, ")", sep="")
    doItAndPrint(command2)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="fit.ar.par")

  mainFrame <- tkframe(top)

  tslist <- Maketslist()
  wvarVariable <- tclVar("")
  tslistCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="7", values=tslist,
                  textvariable=wvarVariable)
  tkgrid(tklabel(mainFrame, text="Pick a time series:", fg="blue"), tslistCBox, sticky="w")
  tkgrid(mainFrame, sticky="w")

  orderVariable <- tclVar("1")
  orderCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="3", values=c("1","2"),
                 textvariable=orderVariable)
  tkgrid(tklabel(mainFrame, text=""))
  tkgrid(tklabel(mainFrame, text="PAR model order = ", fg="blue"), orderCBox, sticky="w")

  tkgrid(mainFrame, sticky="w")

  sinterc <- tkcheckbutton(mainFrame); sintercValue <- tclVar("1")
  strend <- tkcheckbutton(mainFrame); strendValue <- tclVar("0")
  tkgrid(tklabel(mainFrame, text="\nDeterministic components:", fg="blue"), sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal intercepts"), sinterc, sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal trends"), strend, sticky="w")

  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  dialogSuffix(rows=3, columns=2)  ##~ Ver.
}

Makefit.piar <- function()
{
  initializeDialog(title="Fit PIAR model")

  onOK <- function()
  {
    ref <- tclvalue(tkcmd(tslistCBox,"getvalue"))
    tslist <- Maketslist()
    wvar <- tslist[as.numeric(ref)+1]

    dc1 <- tclvalue(sintercValue)
    dc2 <- tclvalue(strendValue)
    p <- tclvalue(orderVariable)

    if (GrabFocus()) tkgrab.release(top)
    tkdestroy(top)

    command1 <- paste("detcomp <- list(regular=c(0,0,0), seasonal=c(", dc1, ",", dc2, "), regvar=0)", sep="")
    justDoIt(command1)
    command2 <- paste("fit.piar(wts=", wvar, ", detcomp=detcomp, p=", p, ")", sep="")
    doItAndPrint(command2)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="fit.piar")

  mainFrame <- tkframe(top)

  tslist <- Maketslist()
  wvarVariable <- tclVar("")
  tslistCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="7", values=tslist,
                  textvariable=wvarVariable)
  tkgrid(tklabel(mainFrame, text="Pick a time series:", fg="blue"), tslistCBox, sticky="w")
  tkgrid(mainFrame, sticky="w")

  orderVariable <- tclVar("1")
  orderCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="3", values=c("1","2"),
                 textvariable=orderVariable)
  tkgrid(tklabel(mainFrame, text=""))
  tkgrid(tklabel(mainFrame, text="PAR model order = ", fg="blue"), orderCBox, sticky="w")

  tkgrid(mainFrame, sticky="w")

  sinterc <- tkcheckbutton(mainFrame); sintercValue <- tclVar("1")
  strend <- tkcheckbutton(mainFrame); strendValue <- tclVar("0")
  tkgrid(tklabel(mainFrame, text="\nDeterministic components:", fg="blue"), sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal intercepts"), sinterc, sticky="w")
  tkgrid(tklabel(mainFrame, text="  Seasonal trends"), strend, sticky="w")

  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  dialogSuffix(rows=3, columns=2)  ##~ Ver.
}

Makepredictpiar <- function()
{
  initializeDialog(title="Predict PIAR model")

  onOK <- function()
  {
    ref <- tclvalue(tkcmd(tslistCBox,"getvalue"))
    tslist <- Maketslist()
    wvar <- tslist[as.numeric(ref)+1]

    p <- tclvalue(orderVariable)
    hpred <- tclvalue(hpredVariable)

    if (GrabFocus()) tkgrab.release(top)
    tkdestroy(top)

    command1 <- paste("predictpiar(wts=", wvar, ", p=", p, ", hpred=", hpred, ")", sep="")
    doItAndPrint(command1)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject="predictpiar")

  mainFrame <- tkframe(top)

  tslist <- Maketslist()
  wvarVariable <- tclVar("")
  tslistCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="7", values=tslist,
                  textvariable=wvarVariable)
  tkgrid(tklabel(mainFrame, text="Pick a time series:", fg="blue"), tslistCBox, sticky="w")
  tkgrid(mainFrame, sticky="w")

  hpredVariable <- tclVar("")
  hpredEntry <- tkentry(mainFrame, width="7", textvariable=hpredVariable)
  tkgrid(tklabel(mainFrame, text="\nType the number of observations to forecast: ", fg="blue"), sticky="w")
  tkgrid(tklabel(mainFrame, text="(A multiple of the periodicity of the data) ", fg="blue"), hpredEntry,
         sticky="w")

  orderVariable <- tclVar("1")
  orderCBox <- tkwidget(mainFrame, "ComboBox", editable=FALSE, width="3", values=c("1","2"),
                 textvariable=orderVariable)
  tkgrid(tklabel(mainFrame, text=""))
  tkgrid(tklabel(mainFrame, text="PAR model order = ", fg="blue"), orderCBox, sticky="w")

  tkgrid(buttonsFrame, columnspan=2, sticky="w")

  dialogSuffix(rows=3, columns=2)  ##~ Ver.
}