import React, { FC } from 'react';
import './PrereqTree.scss';
import { Grid, Popup } from 'semantic-ui-react';

import { PrerequisiteJSONNode, PrerequisiteJSON, CourseData } from '../../types/types';

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
  prerequisiteNames: string[];
  prerequisiteJSON: PrerequisiteJSONNode;
  key?: string;
  index?: number;
}

const Tree: FC<TreeProps> = (props) => {
  let prerequisite = props.prerequisiteJSON;
  let isValueNode = typeof prerequisite === 'string';

  // if value is a string, render leaf node
  if (isValueNode) {
    return (
      <li key={props.index} className={'prerequisite-node'}>
        {/* TODO: content is just duplicate of label, maybe do a graphql query for course names */}
        <Node label={prerequisite as string} content={prerequisite as string} node={'prerequisite-node'} />
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

interface PrereqProps extends CourseData {
}

const PrereqTree: FC<PrereqProps> = (props) => {
  console.log(' The props that came to pre-req tree are: ', props);
  let hasPrereqs = props.prerequisite_tree !== '';
  let hasDependencies = Object.keys(props.prerequisite_for).length !== 0;

  if (props.id === undefined) return <></>;
  else if (!hasPrereqs && !hasDependencies)
    return (
      <div className='missing-tree'>
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
            margin: 'auto',
          }}
        >
          {/* Display dependencies */}
          {hasDependencies && (
            <>
              <ul style={{ padding: '0', display: 'flex' }}>
                <div className='dependency-list-branch'>
                  {props.prerequisite_for.map(
                    (dependency, index) => (
                      <li key={`dependency-node-${index}`} className={'dependency-node'}>
                        {/* TODO: content is just duplicate of label, maybe do a graphql query for course names */}
                        <Node label={dependency} content={dependency} node={'dependency-node'} />
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
        <div
          style={{
            padding: '1em',
            backgroundColor: '#f5f5f5',
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