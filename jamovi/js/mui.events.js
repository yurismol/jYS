const events = {
  onChange_met: function(ui) {
    //console.log(ui.outlcheck.value())
    if (ui.UImethod.value()!=="TGR") ui.isTGR.setValue(false)
    if (ui.UImethod.value()!=="noUI") ui.youden.setValue(false)
    if (ui.minSe.value()<0.75 || ui.minSe.value()>0.99) ui.minSe.setValue(0.9)
    if (ui.minSp.value()<0.75 || ui.minSp.value()>0.99) ui.minSp.setValue(0.9)
    if (ui.uiSe.value()<0.51  || ui.uiSe.value()>0.8) ui.uiSe.setValue(0.55)
    if (ui.uiSp.value()<0.51  || ui.uiSp.value()>0.8) ui.uiSp.setValue(0.55)
  }
};

module.exports = events;
