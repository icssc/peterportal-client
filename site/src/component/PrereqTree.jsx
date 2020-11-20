import React from "react";
import "./PrereqTree.scss";
import { Grid, Divider,Popup } from "semantic-ui-react";

const horizontalStyle = { display: "inline-flex", flexDirection: "row" };

function Node(props) {
    return (
        <div style={{ padding: "1px 0"}} className={`node-container ${props.node}`} key={props.index}>
          <Popup
              trigger = {
                  <a href={ "/course/" + props.label.replace(/\s+/g, "")} role="button" style={{padding: "0.5rem"}} className={"node ui button"}>
                      {props.label}
                  </a>
              }
              content={props.content} basic position="top center" wide="very" />
        </div>
    );
}

function Tree(props) {
    let prerequisite = props.prerequisiteJSON;
    let isValueNode = typeof prerequisite === "string";

    if (isValueNode) {
        return (
            <li key={props.index} className={"prerequisite-node"}>
              <Node label={prerequisite} content={props.prerequisiteNames[prerequisite]} node={"prerequisite-node"}/>
            </li>
        )
    } else if (!isValueNode) {
        return (
            <div style={{ margin: "auto 0"}} className={"prerequisite-node"}>
                <div style={{ display: "inline-flex", flexDirection: "row", padding: "0.5rem 0"}}>
                    <span style={{ margin: "auto" }}>
                        <div className="prereq-branch">
                            {prerequisite.hasOwnProperty("OR") ? "one of" : "all of"}
                        </div>
                    </span>
                    <div className="prereq-clump">
                      <ul className="prereq-list">
                              {prerequisite[Object.keys(prerequisite)[0]].map(
                                  (child, index) => (
                                      <Tree prerequisiteNames={props.prerequisiteNames} index={index} prerequisiteJSON={child}/>      
                                  )
                              )}
                          </ul>
                    </div>
                </div>
            </div>
        )
    }
}
  

function PrereqTree(props) {
    let hasPrereqs = props.prerequisite_tree !== "";
    let hasDependencies = Object.keys(props.dependencies).length !== 0;

    if (props.id === undefined) return "";
    else if (!hasPrereqs && !hasDependencies)
      return (
        <div>
          <span>No Dependencies or Prerequisites!</span>
        </div>
      );
    return (
      <div>
        <Grid.Row className="feature-label">
          <h2><span role="img" aria-label="seedling">🌱</span> Prerequisite Tree</h2>
          <Divider />
        </Grid.Row>

        <Grid.Row className="prereq">
          <div
            style={{
              display: "inline-flex",
              flexDirection: "row",
              margin: "auto",
            }}
          >
            {/* Display dependencies */}
            {hasDependencies && (
              <>
               
                  <ul style={{ padding: "0", display: "flex"}}>
                    <div className="dependency-list-branch">
                      {Object.keys(props.dependencies).map(
                        (dependency, index) => (
                            <li key={props.index} className={"dependency-node"}>
                              <Node label={props.dependencies[dependency]} node={"dependency-node"}/>
                            </li>
                        )
                      )}
                    </div>
                  </ul>
                
                <div style={horizontalStyle}>
                  <span style={{ margin: "auto 11px" }}>
                    <div className="dependency-node dependency-branch">
                      <p>needs</p>
                    </div>
                  </span>
                </div>
              </>
            )}
            {!hasDependencies}
            {/* Display the class id */}

            <Node label={props.id} content={props.title} node={"course-node"}/>

            {/* Spawns the root of the prerequisite tree */}
            {hasPrereqs && (
              <div style={{ display: "flex" }}>
                <Tree
                  prerequisiteNames={props.prerequisite_list}
                  prerequisiteJSON={JSON.parse(props.prerequisite_tree)}
                />

              </div>
            )}
            {!hasPrereqs}
          </div>
          <div
            style={{
              padding: "1em",
              backgroundColor: "#f5f5f5",
              marginTop: "2em",
            }}
          >
            <p>
              {props.prerequisite_text !== "" && <b>Prerequisite: </b>}
              {props.prerequisite_text}
            </p>
          </div>
          </Grid.Row>
      </div>
    );
}

export default PrereqTree;
