import React, { FC } from 'react';
import './PrereqTree.scss';
import { Grid, Popup } from 'semantic-ui-react';

import { PrerequisiteJSONNode, PrerequisiteJSON, CourseGQLData, CourseLookup } from '../../types/types';

interface NodeProps {
  node: string;
  label: string;
  content: string;
  index?: number;
}

const Node: FC<NodeProps> = (props) => {
  return (
    <div style={{ padding: '1px 0' }} className={`node-container ${props.node}`} key={props.index}>
      <Popup
        trigger={
          <a href={'/course/' + props.label.replace(/\s+/g, '')} role='button' style={{ padding: '0.5rem' }} className={'node ui button'}>
            {props.label}
          </a>
        }
        content={props.content} basic position='top center' wide='very' />
    </div>
  );
}

interface TreeProps {
  prerequisiteNames: CourseLookup;
  prerequisiteJSON: PrerequisiteJSONNode;
  key?: string;
  index?: number;
}

const Tree: FC<TreeProps> = (props) => {
  let prerequisite = props.prerequisiteJSON;
  let isValueNode = typeof prerequisite === 'string';

  // if value is a string, render leaf node
  if (isValueNode) {
    let id = (prerequisite as string).replace(/\s+/g, '');
    let content = prerequisite;
    if (props.prerequisiteNames.hasOwnProperty(id)) {
      content = props.prerequisiteNames[id].title;
    }
    return (
      <li key={props.index} className={'prerequisite-node'}>
        <Node label={prerequisite as string} content={content as string} node={'prerequisite-node'} />
      </li>
    )
  }
  // if value is an object, render the rest of the sub tree
  else {
    return (
      <div style={{ margin: 'auto 0' }} className={'prerequisite-node'}>
        <div style={{ display: 'inline-flex', flexDirection: 'row', padding: '0.5rem 0' }}>
          <span style={{ margin: 'auto' }}>
            <div className='prereq-branch'>
              {prerequisite.hasOwnProperty('OR') ? 'one of' : 'all of'}
            </div>
          </span>
          <div className='prereq-clump'>
            <ul className='prereq-list'>
              {(prerequisite as PrerequisiteJSON)[Object.keys(prerequisite)[0]].map(
                (child, index) => (
                  <Tree key={`tree-${index}`} prerequisiteNames={props.prerequisiteNames} index={index} prerequisiteJSON={child} />
                )
              )}
            </ul>
          </div>
        </div>
      </div>
    )
  }
}

interface PrereqProps extends CourseGQLData {
}

const PrereqTree: FC<PrereqProps> = (props) => {
  let hasPrereqs = props.prerequisite_tree !== '';
  let hasDependencies = Object.keys(props.prerequisite_for).length !== 0;

  if (props.id === undefined) return <></>;
  else if (!hasPrereqs && !hasDependencies)
    return (
      <div className='prereq-text-box'>
        <p>
          No Dependencies or Prerequisites!
        </p>
      </div>
    );
  return (
    <div>
      <Grid.Row className='prereq'>
        <div
          style={{
            display: 'inline-flex',
            flexDirection: 'row',
            width: 'fit-content',
            justifyContent: 'center',
            margin: 'auto'
          }}
        >
          {/* Display dependencies */}
          {hasDependencies && (
            <>
              <ul style={{ padding: '0', display: 'flex' }}>
                <div className='dependency-list-branch'>
                  {Object.values(props.prerequisite_for).map(
                    (dependency, index) => (
                      <li key={`dependency-node-${index}`} className={'dependency-node'}>
                        <Node label={dependency.department + ' ' + dependency.number} content={dependency.title} node={'dependency-node'} />
                      </li>
                    )
                  )}
                </div>
              </ul>

              <div style={{ display: 'inline-flex', flexDirection: 'row', marginLeft: '0.5rem' }}>
                <span style={{ margin: 'auto 1rem' }}>
                  <div className='dependency-needs dependency-branch'>
                    needs
                  </div>
                </span>
              </div>
            </>
          )}

          {/* {!hasDependencies && <div className='dependency-branch'>
            <p className='missing-tree'>
              No Dependencies!
            </p>
          </div>} */}

          {/* Display the class id */}
          <Node label={props.id} content={props.title} node={'course-node'} />

          {/* Spawns the root of the prerequisite tree */}
          {hasPrereqs && (
            <div style={{ display: 'flex' }}>
              <Tree
                prerequisiteNames={props.prerequisite_list}
                prerequisiteJSON={JSON.parse(props.prerequisite_tree)}
              />

            </div>
          )}

          {/* {!hasPrereqs && <div className='dependency-branch'>
            <p className='missing-tree'>
              No Prerequisites!
            </p>
          </div>} */}

        </div>
        <div className='prereq-text-box'
          style={{
            padding: '1em',
            marginTop: '2em',
          }}
        >
          <p>
            {props.prerequisite_text !== '' && <b>Prerequisite: </b>}
            {props.prerequisite_text}
          </p>
        </div>
      </Grid.Row>
    </div>
  );
}

export default PrereqTree;