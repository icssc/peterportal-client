import { FC } from 'react';
import './PrereqTree.scss';
import type { Prerequisite, PrerequisiteTree } from '@peterportal/types';
import type { CourseGQLData, CourseLookup } from '../../types/types';
import { Link } from 'react-router-dom';
import { OverlayTrigger, Popover } from 'react-bootstrap';

interface NodeProps {
  label: string;
  content: string;
}

const Node: FC<NodeProps> = ({ label, content }) => {
  const popover = (
    <Popover id="tree-node-popover" className="tree-node-popover" placement="bottom">
      <Popover.Content>{content || label}</Popover.Content>
    </Popover>
  );
  return (
    <OverlayTrigger overlay={popover}>
      {!label.startsWith('AP ') ? (
        <Link to={'/course/' + label.split('(')[0].replace(/\s+/g, '')} role="button" className="node">
          {label}
        </Link>
      ) : (
        <button className="node">{label}</button>
      )}
    </OverlayTrigger>
  );
};

interface PrereqLeafNodeProps {
  prereq: Prerequisite;
  prereqNames: CourseLookup;
}

const PrereqLeafNode: FC<PrereqLeafNodeProps> = ({ prereq, prereqNames }) => {
  const nodeLabel =
    prereq.prereqType === 'course'
      ? `${prereq.courseId} ${prereq.coreq ? '(coreq)' : prereq.minGrade ? `(min grade = ${prereq.minGrade})` : ''}`
      : `${prereq.examName} ${prereq.minGrade ? `(min grade = ${prereq.minGrade})` : ''}`;
  const nodeContent =
    prereqNames[prereq.prereqType === 'course' ? prereq.courseId.replace(/ /g, '') : (prereq.examName ?? '')]?.title ??
    '';
  return <Node label={nodeLabel} content={nodeContent} />;
};

interface PrereqInternalNodeProps {
  prereqTree: PrerequisiteTree;
  prereqNames: CourseLookup;
}

const PrereqInternalNode: FC<PrereqInternalNodeProps> = ({ prereqTree, prereqNames }) => {
  const prereqTreeType = Object.keys(prereqTree)[0] as keyof PrerequisiteTree;
  const prereqChildren = prereqTree[prereqTreeType]!;

  // converts "[course] - all of - one of - [prereq]" to "[course] - one of - [prereq]"
  if (prereqTree.AND && prereqChildren.length === 1 && 'OR' in prereqChildren[0]) {
    return <PrereqInternalNode prereqTree={prereqChildren[0]} prereqNames={prereqNames} />;
  }

  const phraseMapping = {
    AND: 'all of',
    OR: 'one of',
    NOT: 'none of',
  };

  return (
    <>
      <div className="prereq-branch">{phraseMapping[prereqTreeType]}</div>
      <div className="prereq-clump">
        <ul>
          {prereqChildren.map((child, index) => (
            <div key={`tree-${index}`} className="prerequisite-node">
              {Object.prototype.hasOwnProperty.call(child, 'prereqType') ? (
                <PrereqLeafNode prereq={child as Prerequisite} prereqNames={prereqNames} />
              ) : (
                <PrereqInternalNode prereqTree={child as PrerequisiteTree} prereqNames={prereqNames} />
              )}
            </div>
          ))}
        </ul>
      </div>
    </>
  );
};

interface PrereqTreeProps {
  course: CourseGQLData;
}

const PrereqDependents: FC<PrereqTreeProps> = ({ course }) => {
  return (
    <>
      <ul className="dependent-list">
        <div className="dependent-list-branch">
          {Object.values(course.dependents).map((dependent, index) => (
            <li key={`dependent-node-${index}`} className="dependent-node">
              <Node label={`${dependent.department} ${dependent.courseNumber}`} content={dependent.title} />
            </li>
          ))}
        </div>
      </ul>
      <div className="dependent-needs-container">
        <div className="dependent-needs dependent-branch">needs</div>
      </div>
    </>
  );
};

const PrereqText: FC<PrereqTreeProps> = ({ course }) => {
  return (
    <div className="prereq-text-box">
      <b>Prerequisite: </b>
      {course.prerequisiteText}
    </div>
  );
};

const PrereqTree: FC<PrereqTreeProps> = ({ course }) => {
  const hasPrereqs = JSON.stringify(course.prerequisiteTree) !== '{}';
  const hasDependents = Object.keys(course.dependents).length !== 0;

  if (!hasPrereqs && !hasDependents) {
    return (
      <div className="prereq-text-box">
        <p>No Prerequisites or Dependents!</p>
      </div>
    );
  }

  return (
    <div className="prereq">
      <div className="complete-prereq-tree">
        {/* Display dependents */}
        {hasDependents && <PrereqDependents course={course} />}

        {/* Display the course node itself */}
        <div className="course-node">
          <Node label={`${course.department} ${course.courseNumber}`} content={course.title} />
        </div>

        {/* Display the prerequisite tree, recursively */}
        {hasPrereqs && (
          <div className="prerequisite-node-container">
            <div className="prerequisite-node">
              <PrereqInternalNode prereqTree={course.prerequisiteTree} prereqNames={course.prerequisites} />
            </div>
          </div>
        )}
      </div>
      {/* Display prerequisite text */}
      {course.prerequisiteText && <PrereqText course={course} />}
    </div>
  );
};

export default PrereqTree;
