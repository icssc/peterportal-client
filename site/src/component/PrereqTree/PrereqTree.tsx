import { FC } from 'react';
import './PrereqTree.scss';
import type { Prerequisite, PrerequisiteTree } from '@peterportal/types';
import type { CourseGQLData, CourseLookup } from '../../types/types';
import { Link } from 'react-router-dom';
import { OverlayTrigger, Popover } from 'react-bootstrap';
import { removeWhitespace } from '../../helpers/util';

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
        <Link to={'/course/' + removeWhitespace(label.split('(')[0])} role="button" className="node">
          {label}
        </Link>
      ) : (
        <button className="node">{label}</button>
      )}
    </OverlayTrigger>
  );
};

interface LeafNodeProps {
  prereq: Prerequisite;
  prereqNames: CourseLookup;
}

const LeafNode: FC<LeafNodeProps> = ({ prereq, prereqNames }) => {
  const label =
    prereq.prereqType === 'course'
      ? `${prereq.courseId} ${prereq.coreq ? '(coreq)' : prereq.minGrade ? `(min grade = ${prereq.minGrade})` : ''}`
      : `${prereq.examName} ${prereq.minGrade ? `(min grade = ${prereq.minGrade})` : ''}`;
  const content =
    prereq.prereqType === 'course'
      ? prereqNames[prereq.courseId.replace(/ /g, '')]?.title
      : prereqNames[prereq.examName]?.title;
  return <Node label={label} content={content} />;
};

interface InternalNodeProps {
  prereqTree: PrerequisiteTree;
  prereqNames: CourseLookup;
}

const InternalNode: FC<InternalNodeProps> = ({ prereqTree, prereqNames }) => {
  const prereqTreeType = Object.keys(prereqTree)[0] as keyof PrerequisiteTree;
  const prereqChildren = prereqTree[prereqTreeType]!;

  // converts "[course] - all of - one of - [prereq]" to "[course] - one of - [prereq]"
  if (prereqTree.AND && prereqChildren.length === 1 && 'OR' in prereqChildren[0]) {
    return <InternalNode prereqTree={prereqChildren[0]} prereqNames={prereqNames} />;
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
                <LeafNode prereq={child as Prerequisite} prereqNames={prereqNames} />
              ) : (
                <InternalNode prereqTree={child as PrerequisiteTree} prereqNames={prereqNames} />
              )}
            </div>
          ))}
        </ul>
      </div>
    </>
  );
};

interface PrereqTreeProps {
  data: CourseGQLData;
}

const DependentNodes: FC<PrereqTreeProps> = ({ data }) => {
  return (
    <>
      <ul className="dependent-list">
        <div className="dependent-list-branch">
          {Object.values(data.dependents).map((dependent, index) => (
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

const PrereqTree: FC<PrereqTreeProps> = ({ data }) => {
  const hasPrereqs = JSON.stringify(data.prerequisiteTree) !== '{}';
  const hasDependents = Object.keys(data.dependents).length !== 0;

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
        {hasDependents && <DependentNodes data={data} />}

        {/* Display the starting node */}
        <div className="course-node">
          <Node label={`${data.department} ${data.courseNumber}`} content={data.title} />
        </div>

        {/* Display the prerequisite tree, recursively */}
        {hasPrereqs && (
          <div className="prerequisite-node-container">
            <div className="prerequisite-node">
              <InternalNode prereqTree={data.prerequisiteTree} prereqNames={data.prerequisites} />
            </div>
          </div>
        )}
      </div>
      {/* Display prerequisite text */}
      {data.prerequisiteText && (
        <div className="prereq-text-box">
          <b>Prerequisite: </b>
          {data.prerequisiteText}
        </div>
      )}
    </div>
  );
};

export default PrereqTree;
