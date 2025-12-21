const events = {
  onChange_met: function(ui) {
    //console.log(ui.outlcheck.value())
    if (ui.UImethod.value()!=="TGR") ui.isTGR.setValue(false)
    if (ui.UImethod.value()!=="noUI") ui.youden.setValue(false)
    if (ui.minSe.value()<75 || ui.minSe.value()>99) ui.minSe.setValue(90)
    if (ui.minSp.value()<75 || ui.minSp.value()>99) ui.minSp.setValue(90)
    if (ui.uiSe.value()<51  || ui.uiSe.value()>80) ui.uiSe.setValue(55)
    if (ui.uiSp.value()<51  || ui.uiSp.value()>80) ui.uiSp.setValue(55)
  }
};

module.exports = events;
