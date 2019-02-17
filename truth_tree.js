import { mapFindByObjectIndex, objectCompareByValue } from './helpers.js';
import statementIntoText from './stringify_statement.js';
import ruleNameToLabel from './stringify_rule.js';
import { DataSet, Network } from 'vis/dist/vis-network.min.js';
import './css/truth_tree.css';

export default class TruthTree {
  // nodes in 'data' are guaranteed to be in the order that they should appear and were derived,
  // and that is an implicit assumption behind all this code
  constructor(container, data) {
    this.network = null;

    this.container = container;

    this.css = {
      'statement-text-colour': this._cssValue('--statement-text-colour'),
      'statement-text-size': parseInt(this._cssValue('--statement-text-size')),
      'statement-text-face': this._cssValue('--statement-text-face'),
      'branch-close-icon-face': this._cssValue('--branch-close-icon-face'),
      'branch-close-icon-code': String.fromCharCode(parseInt(this._cssValue('--branch-close-icon-code'), 16)),
      'branch-close-icon-size': parseInt(this._cssValue('--branch-close-icon-size')),
      'branch-close-icon-colour': this._cssValue('--branch-close-icon-colour'),
      'edge-colour': this._cssValue('--edge-colour'),
      'linenumber-text-face': this._cssValue('--linenumber-text-face'),
      'linenumber-text-size': parseInt(this._cssValue('--linenumber-text-size')),
      'linenumber-text-colour': this._cssValue('--linenumber-text-colour'),
      'derivedfrom-text-face': this._cssValue('--derivedfrom-text-face'),
      'derivedfrom-text-size': parseInt(this._cssValue('--derivedfrom-text-size')),
      'derivedfrom-text-colour': this._cssValue('--derivedfrom-text-colour'),
      'checkmark-icon-size': parseInt(this._cssValue('--checkmark-icon-size')),
      'checkmark-icon-face': this._cssValue('--checkmark-icon-face'),
      'checkmark-icon-code': String.fromCharCode(parseInt(this._cssValue('--checkmark-icon-code'), 16)),
      'checkmark-icon-colour': this._cssValue('--checkmark-icon-colour'),
      'level-separation': parseInt(this._cssValue('--level-separation')),
      'node-spacing': parseInt(this._cssValue('--node-spacing')),
    }

    this.options = {
      autoResize: true,
      configure: {
        enabled: false,
      },
      physics: {
        enabled: false,
      },
      manipulation: {
        enabled: false,
      },
      interaction: {
        dragNodes: false,
        dragView: true,
        hover: false,
        hoverConnectedEdges: false,
        keyboard: {
          enabled: true,
          bindToWindow: false,
        },
        selectable: false,
        selectConnectedEdges: false,
      },
      layout: {
        hierarchical: {
          enabled: true,
          levelSeparation: this.css['level-separation'],
          nodeSpacing: this.css['node-spacing'],
          treeSpacing: 0,
          blockShifting: false,
          edgeMinimization: false,
          parentCentralization: false,
          direction: 'UD',
          sortMethod: 'directed',
        },
      },
    };

    this.idCounter = 0;

    this._render(data);
  }

  _parseBranchData(mapIds, branchData, parent) {
    var branch = {};

    branch.parent = parent;

    mapIds.set(branchData.id, branch);

    branch.nodes = branchData.nodes.reduce((nodes, nodeData) => {
      nodes.push(this._parseNodeData(mapIds, nodeData, branch));
      return nodes;
    }, []);

    branch.children = branchData.children.reduce(
      (children, childData) => {
        children.push(this._parseBranchData(mapIds, childData, branch));
        return children;
      },
      []
    );

    branch.closed = branchData.closed;

    return branch;
  }

  _parseNodeData(mapIds, nodeData, ownerBranch) {
    var node = {};

    mapIds.set(nodeData.id, node);

    node.text = statementIntoText(nodeData.statement);

    if (nodeData.derived_from != null) {
      var derivedFromNodeId = nodeData.derived_from.node_id;
      var derivedFromBranchId = nodeData.derived_from.branch_id;
      var derivedWithRule = nodeData.derived_from.rule;
      var derivationId = nodeData.derived_from.derivation_id;

      node.derivedFrom = {
        node: mapFindByObjectIndex(mapIds, derivedFromNodeId),
        branch: mapFindByObjectIndex(mapIds, derivedFromBranchId),
        rule: ruleNameToLabel(derivedWithRule),
        derivationId
      }

      // Mark the statement that this was derived from as done
      // (unless it's an UQ rule, which can be applied
      // infinitely many times)
      if (derivedWithRule != "UniversalQuantifier") {
        node.derivedFrom.node.done = true;
      }
    }
    else {
      node.derivedFrom = null;
    }

    node.done = false;
    node.ownerBranch = ownerBranch;

    node.treeLevel = null;
    node.treeId = null;

    return node;
  }

  _findAllNodesForEachDerivation(mainTrunk) {
    // A derivation can only appear in either one
    // single branch (one or more statements)
    // or spanning sibling branches, but NEVER 
    // more than once in the same tree path but different branches
    var mapDerivations = new Map();

    var stack = [mainTrunk];

    while (stack.length != 0) {
      var branch = stack.pop();

      branch.nodes.forEach(node => {
        if (node.derivedFrom != null) {
          var derivs = mapFindByObjectIndex(mapDerivations, node.derivedFrom.derivationId);
          if (derivs != null) {
            if (!derivs.find(x => x.ownerBranch == node.ownerBranch)) {
              derivs.push(node);
            } else {
              derivs.push(node);
            }
          } else {
            mapDerivations.set(node.derivedFrom.derivationId, [node]);
          }
        }
      });

      stack.push(...branch.children);
    }

    return mapDerivations;
  }

  _offsetLevels(branch, start, offset) {
    branch.nodes.forEach(node => {
      if (node.treeLevel != null && node.treeLevel >= start) {
        node.treeLevel += offset;
      }
    });

    branch.children.forEach(child => {
      this._offsetLevels(child, child.nodes[0].treeLevel, offset);
    });
  }

  _computeLevels(mainTrunk) {
    // 1. Find and group all nodes from each different derivation
    // 2. Iterate over all nodes, assigning it a 'currLevel' (where currLevel is incremented
    // for every node, and resets to the level of the first node of one branch when moving
    // to a sibling branch), over each branch, 
    // and:
    //    3. If it is not a derived statement, assign a level of 'currLevel' and move on, else:
    //    4. Look up in the derivations map  
    //    5. Iterate over all nodes (where its level is 'y') from the same derivation (and same line)
    //    6. If there is some 'y > currLevel', set 'currLevel = y'
    //    7. If there is any 'y < currLevel', (for all) change 'y' to 'currLevel' and increment levels of all 
    //       following nodes in that branch of 'y'
    //    8. if 'y' is not defined yet, assign 'currLevel' to the node
    var mapDerivations = this._findAllNodesForEachDerivation(mainTrunk);

    var currLevel = 0;

    var stack = [[mainTrunk, currLevel]];

    while (stack.length != 0) {
      var [branch, currLevel] = stack.pop();

      branch.nodes.forEach(node => {
        currLevel += 1;

        if (node.derivedFrom == null) {
          node.treeLevel = currLevel;
        } else {
          var derivs = mapFindByObjectIndex(mapDerivations, node.derivedFrom.derivationId);

          derivs.filter(x => objectCompareByValue(x.derivedFrom.derivationId, node.derivedFrom.derivationId)).forEach(x => {
            if (x.treeLevel == null) {
              node.treeLevel = currLevel;
            }
            else if (x.treeLevel > currLevel) {
              currLevel = x.treeLevel;
              node.treeLevel = currLevel;
            }
            else if (x.treeLevel < currLevel) {
              this._offsetLevels(
                x.ownerBranch,
                x.treeLevel,
                currLevel - x.treeLevel
              );
            }
          });
        }
      });

      stack.push(...branch.children.map(x => [x, currLevel + 2]));
      // + 2 for spacing between branches (one for the hidden parent, another for the hidden parent of child branches)
    }
  }

  _compute(data) {
    var mainTrunk = this._parseBranchData(new Map(), data.main_trunk, null);

    var dataSet = {
      nodes: [],
      edges: [],
    };

    this._computeLevels(mainTrunk);
    this._populateDataSet(dataSet, mainTrunk);
    this._computeLineNumbers(dataSet);

    return [{
      nodes: new DataSet(dataSet.nodes),
      edges: new DataSet(dataSet.edges),
    }, mainTrunk];
  }


  _cssValue(property) {
    return window.getComputedStyle(this.container).getPropertyValue(property);
  }

  _insertStatementNodeOnDataSet(dataSet, text, level, node) {
    this.idCounter += 1;
    dataSet.nodes.push({
      id: this.idCounter,
      level,
      shape: 'text',
      label: text,
      font: {
        color: this.css['statement-text-colour'],
        size: this.css['statement-text-size'],
        face: this.css['statement-text-face']
      },
      // own properties
      nodeRef: node, // useful to know which nodes refer to actual statements
      // (there can be hidden nodes & icon nodes as well)
      // and for post-processing of the dataset
      lineNumber: null, // line we'll associate to this node
    });
    return this.idCounter;
  }

  _insertHiddenNodeOnDataSet(dataSet, level) {
    this.idCounter += 1;
    dataSet.nodes.push({
      id: this.idCounter,
      level,
      shape: 'text',
    });
    return this.idCounter;
  }

  _insertBranchClosedNodeOnDataSet(dataSet, level) {
    this.idCounter += 1;
    dataSet.nodes.push({
      id: this.idCounter,
      level,
      shape: 'icon',
      icon: {
        face: this.css['branch-close-icon-face'],
        code: this.css['branch-close-icon-code'],
        size: this.css['branch-close-icon-size'],
        color: this.css['branch-close-icon-colour']
      },
    });
    return this.idCounter;
  }

  _insertHiddenEdgeOnDataSet(dataSet, from, to) {
    dataSet.edges.push({
      from,
      to,
      color: { opacity: 0 }
    });
  }

  _insertEdgeOnDataSet(dataSet, from, to) {
    dataSet.edges.push({
      from,
      to,
      color: { color: this.css['edge-colour'] },
    });
  }

  _populateDataSet(dataSet, branch) {
    // Add all nodes from 'branch' into the dataset, linked by
    // a hidden edge
    var last = branch.nodes.reduce((prev, node) => {
      node.treeId = this._insertStatementNodeOnDataSet(dataSet, node.text, node.treeLevel, node);

      if (prev != null) {
        this._insertHiddenEdgeOnDataSet(dataSet, prev.treeId, node.treeId);
      }

      return node;
    }, null);

    if (branch.closed) {
      // Mark as closed and stop
      var closedIconNodeId = this._insertBranchClosedNodeOnDataSet(dataSet, last.treeLevel + 1);
      this._insertHiddenEdgeOnDataSet(dataSet, last.treeId, closedIconNodeId);
    }
    else {
      // Here we add 1 + n hidden nodes: a parent, and n children
      // These hidden nodes serve the purpose of adding padding
      // between the branches, because vis.js by default connects
      // edges to the very center of the nodes
      // We also need to offset the children branches' nodes' levels by 2
      // (one level for the hidden parent node, one level for all the hidden children nodes)
      // Actually I had to move the offseting to _computeLevels because
      // doing it here would require recomputing levels in some cases
      var hiddenParentId = this._insertHiddenNodeOnDataSet(dataSet, last.treeLevel + 1);
      this._insertHiddenEdgeOnDataSet(dataSet, last.treeId, hiddenParentId);

      branch.children.forEach(child => {
        // this._offsetLevels(child, child.nodes[0].treeLevel, 2);

        // Children may be lower in the tree to match the lines for each derivation
        // hence the hidden parent of the child should be only one level above the child
        // and not last.treeLevel + 2 as you might assume
        var hiddenChildId = this._insertHiddenNodeOnDataSet(dataSet, child.nodes[0].treeLevel - 1);
        this._insertEdgeOnDataSet(dataSet, hiddenParentId, hiddenChildId);

        this._populateDataSet(dataSet, child);

        this._insertHiddenEdgeOnDataSet(dataSet, hiddenChildId, child.nodes[0].treeId);
      });
    }
  }

  _computeLineNumbers(dataSet) {
    // Associate a line number with each level that contains statement nodes,
    // in order from lowest level to highest level
    var visitedLevels = [];

    var lineNumberCounter = 0;

    dataSet.nodes.sort((a, b) => a.level - b.level).forEach(node => {
      if (node.nodeRef != null) {
        var existing = visitedLevels.find(x => x.level == node.level);
        if (existing) {
          node.lineNumber = existing.lineNumber;
        }
        else {
          lineNumberCounter += 1;
          node.lineNumber = lineNumberCounter;
          visitedLevels.push({
            level: node.level,
            lineNumber: lineNumberCounter
          });
        }
      }
    });
  }

  _renderLineNumber(ctx, lineNumber, xRightBound, y) { // x is the rightmost bound for where the text must be rendered
    ctx.font = this.css['linenumber-text-size'].toString().concat('px ', this.css['linenumber-text-face']);
    ctx.fillStyle = this.css['linenumber-text-colour'];
    ctx.textBaseline = 'middle'; // Absolutely critical

    var text = lineNumber.toString().concat('.');
    var textWidth = ctx.measureText(text).width;

    ctx.fillText(text, xRightBound - textWidth, y);

    return textWidth;
  }

  _renderDoneCheckMark(ctx, xRightBound, y) { // see _renderLineNumber for xRightBound
    ctx.font = this.css['checkmark-icon-size'].toString().concat('px ', this.css['checkmark-icon-face']);
    ctx.fillStyle = this.css['checkmark-icon-colour'];

    var text = this.css['checkmark-icon-code'];
    var textWidth = ctx.measureText(text).width;

    ctx.fillText(text, xRightBound - textWidth, y);

    return textWidth;
  }

  _renderLeftGutter(ctx, dataSet) {
    var treeBoundingBox = this._getTreeBoundingBox(dataSet);

    // Draw line numbers from ]-infinity, treeBoundingBox.left] and check marks
    // for nodes that have had a rule applied to them already (except
    // for UQ)
    var maxWidthUsed = 0;

    var distinctLevels = dataSet.nodes.distinct('level');

    distinctLevels.forEach(level => {
      // Find first for some level 'level' (left gutter is the same for all in the
      // same level)
      var node = dataSet.nodes.get({
        filter: x => {
          return x.level === level;
        }
      }).find(x => x.nodeRef != null);

      if (node) {
        // All nodes on same level are on same Y
        var nodeOriginY = this._calculateNodeOriginY(node.id);

        var lineNumberTextWidth = this._renderLineNumber(
          ctx,
          node.lineNumber,
          treeBoundingBox.left,
          nodeOriginY
        );

        var checkMarkWidth = 0;

        if (node.nodeRef.done) {
          checkMarkWidth = this._renderDoneCheckMark(
            ctx,
            treeBoundingBox.left - lineNumberTextWidth - 5, // - 5 for spacing
            nodeOriginY
          );
        }

        maxWidthUsed = Math.max(maxWidthUsed, lineNumberTextWidth + checkMarkWidth);
      }
    });

    return maxWidthUsed;
  }

  // Calculates the position on the Y axis of the origin of any given node
  // (canvas space)
  _calculateNodeOriginY(nodeId) {
    var boundingBox = this.network.getBoundingBox(nodeId);
    return (boundingBox.top + boundingBox.bottom) / 2;
  }

  // Renders '<derived from ...>' data on every line
  _renderRightGutter(ctx, dataSet) {
    var treeBoundingBox = this._getTreeBoundingBox(dataSet);

    // At any one level, all statements are derived from the same one
    // statement, hence derivation data will be the same
    var data = {};

    dataSet.nodes.get({
      filter: x => x.nodeRef != null && x.nodeRef.derivedFrom != null
    }).forEach(node => {
      if (!Object.keys(data).find(x => x == node.nodeRef.treeLevel)) {
        data[node.level] = { nodeId: node.id, derivedFrom: node.nodeRef.derivedFrom };
      }
    });

    // Render
    ctx.font = this.css['derivedfrom-text-size'].toString().concat('px ', this.css['derivedfrom-text-face']);
    ctx.fillStyle = this.css['derivedfrom-text-colour'];
    ctx.textBaseline = 'middle'; // Absolutely critical (in order to really center the text)

    var maxWidthUsed = 0;

    Object.values(data).forEach(levelData => {
      // Get the center Y position for this level
      var y = this._calculateNodeOriginY(levelData.nodeId);

      var lineNumber = dataSet.nodes.get(levelData.derivedFrom.node.treeId).lineNumber;
      var rule = levelData.derivedFrom.rule;

      var text = ['(', lineNumber, ', ', rule, ')'].join('');

      ctx.fillText(text, treeBoundingBox.right, y);

      maxWidthUsed = Math.max(maxWidthUsed, ctx.measureText(text).width);
    });

    return maxWidthUsed;
  }

  _getTreeBoundingBox(dataSet) {
    var boundingBox = {
      left: 1e9,
      top: 1e9, // Same
      right: -1e9, // Some random large negative value
      bottom: -1e9,
    };

    dataSet.nodes.forEach(node => {
      var networkBoundingBox = this.network.getBoundingBox(node.id);

      boundingBox.right = Math.max(boundingBox.right, networkBoundingBox.right);
      boundingBox.left = Math.min(boundingBox.left, networkBoundingBox.left);
      boundingBox.bottom = Math.max(boundingBox.bottom, networkBoundingBox.bottom);
      boundingBox.top = Math.min(boundingBox.top, networkBoundingBox.top);
    });

    return boundingBox;
  }

  _render(data) {
    var [dataSet, mainTrunk] = this._compute(data);

    // If the div container does not have a width/height > 0,
    // the canvas will be in a failed state and we won't
    // be able to render
    if (this.container.style.width == 0 || this.container.style.height == 0) {
      this.container.style.width = '10px';
      this.container.style.height = '10px';
    }

    this.network = new Network(this.container, dataSet, this.options);

    var alreadyAdjusted = false;

    this.network.on('afterDrawing', ctx => {
      // Neither gutter ever increases the height of the tree
      // so we only take into account the additional width
      var leftGutterWidth = this._renderLeftGutter(ctx, dataSet);

      var rightGutterWidth = this._renderRightGutter(
        ctx,
        dataSet
      );

      // I don't think there's any better way to do this.
      // In order to adapt the container's size to the tree size,
      // we need to first render the tree, then get its max size,
      // update the container's size, and rerender. All the computations
      // are already done - it's really just redrawing.
      // I can adjust max-height and max-width for the container
      // so that it won't overflow.
      if (!alreadyAdjusted) {
        alreadyAdjusted = true;

        var boundingBox = this._getTreeBoundingBox(dataSet);

        var treeWidth = boundingBox.right - boundingBox.left;
        var treeHeight = boundingBox.bottom - boundingBox.top;

        treeWidth += leftGutterWidth + rightGutterWidth;

        treeWidth = Math.ceil(treeWidth);
        treeHeight = Math.ceil(treeHeight);

        this.container.style.width = treeWidth.toString() + 'px';
        this.container.style.height = treeHeight.toString() + 'px';

        this.network.redraw(); // This seems to be rerendering
        // but it's actually needed, otherwise the network will simply
        // disappear for some reason
        // FIXME?
        this.network.fit(); // This doesn't actually take into account
        // any post-rendering
      }
    });
  }
}

// Mock
// new TruthTree().render({
//   main_trunk: {
//     id: 0,
//     closed: false,
//     nodes: [
//       {
//         id: 20,
//         statement: {
//           LogicalDisjunction: [
//             {
//               Simple: ['A', null],
//             },
//             {
//               Simple: ['B', null]
//             }
//           ]
//         },
//         derived_from: null,
//       },
//       {
//         id: 21,
//         statement: {
//           Simple: ['D', null]
//         },
//         derived_from: null,
//       },
//       {
//         id: 24,
//         statement: {
//           LogicalConjunction: [
//             {
//               Simple: ['D', null]
//             },
//             {
//               Simple: ['E', null]
//             }
//           ],
//           derived_from: null,
//         },
//       }
//     ],
//     children: [
//       {
//         id: 1,
//         closed: true,
//         nodes: [
//           {
//             id: 22,
//             statement: {
//               Simple: ['A', null]
//             },
//             derived_from: {
//               node_id: 20,
//               branch_id: 0,
//               rule: 'Disjunction',
//               derivation_id: {
//                 id: 1,
//                 index: 1
//               },
//             },
//           },
//           {
//             id: 25,
//             statement: {
//               Simple: ['D', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 1,
//               },
//             }
//           },
//           {
//             id: 26,
//             statement: {
//               Simple: ['E', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 2
//               },
//             }
//           }
//         ],
//         children: [

//         ]
//       },
//       {
//         id: 2,
//         closed: true,
//         nodes: [
//           {
//             id: 23,
//             statement: {
//               Simple: ['B', null]
//             },
//             derived_from: {
//               node_id: 20,
//               branch_id: 0,
//               rule: 'Disjunction',
//               derivation_id: {
//                 id: 1,
//                 index: 1,
//               },
//             },
//           },
//           {
//             id: 27,
//             statement: {
//               Simple: ['G', null]
//             },
//             derived_from: null,
//           },
//           {
//             id: 25,
//             statement: {
//               Simple: ['D', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 1,
//               },
//             }
//           },
//           {
//             id: 26,
//             statement: {
//               Simple: ['E', null]
//             },
//             derived_from: {
//               node_id: 24,
//               branch_id: 0,
//               rule: 'Conjunction',
//               derivation_id: {
//                 id: 2,
//                 index: 2,
//               },
//             }
//           }
//         ],
//         children: [

//         ]
//       },
//     ]
//   }
// });