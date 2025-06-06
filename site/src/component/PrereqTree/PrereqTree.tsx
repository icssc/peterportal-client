import { FC } from 'react';
import { Link } from 'react-router-dom';
import { OverlayTrigger, Popover } from 'react-bootstrap';
import './PrereqTree.scss';

import type { CourseGQLData, CourseLookup } from '../../types/types';
import type { Prerequisite, PrerequisiteTree, PrerequisiteNode } from '@peterportal/types';

const phraseMapping = {
  AND: 'all of',
  OR: 'one of',
  NOT: 'none of',
};

interface NodeProps {
  node: string;
  label: string;
  content: string;
  index?: number;
}

const Node: FC<NodeProps> = (props) => {
  const popover = (
    <Popover id="tree-node-popover" className="tree-node-popover" placement="bottom">
      <Popover.Content>{props.content ? props.content : props.label}</Popover.Content>
    </Popover>
  );
  return (
    <div style={{ padding: '1px 0' }} className={`node-container ${props.node}`} key={props.index}>
      <OverlayTrigger overlay={popover}>
        {!props.label.startsWith('AP ') ? (
          <Link to={'/course/' + props.label.split('(')[0].replace(/\s+/g, '')} role="button" className="node">
            {props.label}
          </Link>
        ) : (
          <button className="node">{`${props.label}`}</button>
        )}
      </OverlayTrigger>
    </div>
  );
};

interface TreeProps {
  prerequisiteNames: CourseLookup;
  prerequisiteJSON: PrerequisiteNode;
  key?: string;
  index?: number;
}

const PrereqTreeNode: FC<TreeProps> = (props) => {
  const prerequisite = props.prerequisiteJSON;
  const isValueNode = Object.prototype.hasOwnProperty.call(prerequisite, 'prereqType');

  // if value is a string, render leaf node
  if (isValueNode) {
    const prereq = prerequisite as Prerequisite;
    return (
      <li key={props.index} className="prerequisite-node">
        <Node
          label={
            prereq.prereqType === 'course'
              ? `${prereq.courseId} ${prereq.coreq ? '(coreq)' : prereq.minGrade ? `(min grade = ${prereq.minGrade})` : ''}`
              : `${prereq.examName} ${prereq.minGrade ? `(min grade = ${prereq.minGrade})` : ''}`
          }
          content={
            props.prerequisiteNames[
              prereq.prereqType === 'course' ? prereq.courseId.replace(/ /g, '') : (prereq.examName ?? '')
            ]?.title ?? ''
          }
          node="prerequisite-node"
        />
      </li>
    );
  }

  // if value is an object, render the rest of the sub tree
  const prereqTree = prerequisite as PrerequisiteTree;
  const prereqTreeType = Object.keys(prereqTree)[0] as keyof PrerequisiteTree;
  const prereqChildren = prereqTree[prereqTreeType] as PrerequisiteTree[];

  if (prereqTree.AND && prereqChildren.length === 1 && prereqChildren[0].OR) {
    return <PrereqTreeNode prerequisiteNames={props.prerequisiteNames} prerequisiteJSON={prereqChildren[0]} />;
  }

  return (
    <div style={{ margin: 'auto 0' }} className="prerequisite-node">
      <div style={{ display: 'inline-flex', flexDirection: 'row', padding: '0.5rem 0' }}>
        <span style={{ margin: 'auto' }}>
          <div className="prereq-branch">{phraseMapping[prereqTreeType]}</div>
        </span>
        <div className="prereq-clump">
          <ul className="prereq-list">
            {prereqChildren.map((child, index) => (
              <PrereqTreeNode
                key={`tree-${index}`}
                prerequisiteNames={props.prerequisiteNames}
                index={index}
                prerequisiteJSON={child}
              />
            ))}
          </ul>
        </div>
      </div>
    </div>
  );
};

interface PrereqTreeProps {
  course: CourseGQLData;
}

const PrereqTree: FC<PrereqTreeProps> = ({ course }) => {
  const hasPrereqs = JSON.stringify(course.prerequisiteTree) !== '{}';
  const hasDependents = Object.keys(course.dependents).length !== 0;

  if (course.id === undefined) return null;

  if (!hasPrereqs && !hasDependents) {
    return (
      <div className="prereq-text-box">
        <p>No Prerequisites or Dependents!</p>
      </div>
    );
  }

  return (
    <div>
      <div className="prereq">
        <div
          style={{
            display: 'inline-flex',
            flexDirection: 'row',
            width: 'fit-content',
            justifyContent: 'center',
            margin: 'auto',
          }}
        >
          {/* Display dependents */}
          {hasDependents && (
            <>
              <ul style={{ padding: '0', display: 'flex' }}>
                <div className="dependent-list-branch">
                  {Object.values(course.dependents).map((dependent, index) => (
                    <li key={`dependent-node-${index}`} className="dependent-node">
                      <Node
                        label={`${dependent.department} ${dependent.courseNumber}`}
                        content={dependent.title}
                        node="dependent-node"
                      />
                    </li>
                  ))}
                </div>
              </ul>

              <div style={{ display: 'inline-flex', flexDirection: 'row', marginLeft: '0.5rem' }}>
                <span style={{ margin: 'auto 1rem' }}>
                  <div className="dependent-needs dependent-branch">needs</div>
                </span>
              </div>
            </>
          )}

          {/* {!hasDependents && <div className='dependent-branch'>
            <p className='missing-tree'>
              No Dependents!
            </p>
          </div>} */}

          {/* Display the class id */}
          <Node label={`${course.department} ${course.courseNumber}`} content={course.title} node="course-node" />

          {/* Spawns the root of the prerequisite tree */}
          {hasPrereqs && (
            <div style={{ display: 'flex' }}>
              <PrereqTreeNode prerequisiteNames={course.prerequisites} prerequisiteJSON={course.prerequisiteTree} />
            </div>
          )}

          {/* {!hasPrereqs && <div className='dependent-branch'>
            <p className='missing-tree'>
              No Prerequisites!
            </p>
          </div>} */}
        </div>
        {course.prerequisiteText !== '' && (
          <div
            className="prereq-text-box"
            style={{
              padding: '1em',
              marginTop: '2em',
            }}
          >
            <p>
              <b>Prerequisite: </b>
              {course.prerequisiteText}
            </p>
          </div>
        )}
      </div>
    </div>
  );
};

export default PrereqTree;
