const events = {
    update: function(ui) {
        console.log("Updating ...")
        calcModelTerms(ui, this);
        updateLevelControls(ui, this);
    },

    onChange_group: function(ui) {
        console.log("Changing ...")
        calcModelTerms(ui, this);
        updateLevelControls(ui, this);
    },

    onChange_refLevels: function(ui) {
        console.log("Levels ...")
        updateLevelControls(ui, this);
    }
};

var updateLevelControls = function(ui, context) {
    let dlist = ui.refLevels.value();
    let list = ui.refLevels.applyToItems(0, (item, index, column) => {
        if (column === 1)
            item.setPropertyValue('variable', dlist[index].var );
    });
};

var updateContrasts = function(ui, variableList, context) {
    var currentList = context.clone(ui.refLevels.value(), []);
    //console.log(currentList)

    var list3 = [];

    let found = null;
    for (let j = 0; j < currentList.length; j++) {
        if (currentList[j].var === variableList) {
            found = currentList[j];
            break;
        }
    }
    if (found === null)
        list3.push({ var: variableList, ref: null });
    else
        list3.push(found);

    ui.refLevels.setValue(list3);
};

var calcModelTerms = function(ui, context) {
    var variableList = context.clone(ui.group.value(), []);
    updateContrasts(ui, variableList, context);
};

module.exports = events;
