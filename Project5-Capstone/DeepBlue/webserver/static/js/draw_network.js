
  
var network;
// create a network
var container = document.getElementById('mynetwork');
var data = {
  nodes: nodes,
  edges: edges
};
var options = {
  nodes: {
    shape: 'dot',
    scaling: {
      min: 2,
      max: 4
    },
    font: {
      size: 2,
      face: 'Tahoma'
    }
  },
  // layout: {
  //   improvedLayout: false
  // },

  edges: {
    width: 1,
    color: {inherit: 'from'},
    color: 'red',
    smooth: {
      type: 'continuous'
    }
  },
  physics: {
    stabilization: false,
    barnesHut: {
      gravitationalConstant: -80000,
      springConstant: 0.001,
      springLength: 200
    }
  },
  interaction: {
    tooltipDelay: 200,
    hideEdgesOnDrag: true
  }
};
network = new vis.Network(container, data, options);




