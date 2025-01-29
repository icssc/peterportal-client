import './RequiredCourseList.scss';
import React, { FC } from 'react';
import {
  collapseSingletonRequirements,
  ProgramRequirement,
  TypedProgramRequirement,
} from '../../../helpers/courseRequirements';
import { CaretRightFill } from 'react-bootstrap-icons';
// import Course from '../Course';
// import CoursePopover from '../../../component/CoursePopover/CoursePopover';

const CourseTile: FC<{ courseID: string }> = ({ courseID }) => {
  return (
    <div className="course-tile">
      {courseID}
      {/* <CoursePopover course={{}} /> */}
      {/* popover MAYBE */}
    </div>
  );
};

const GroupHeader: FC<{ title: string }> = ({ title }) => {
  return (
    <div className="group-header">
      <CaretRightFill />
      <b>{title}</b>
    </div>
  );
};

const CourseRequirement: FC<{ data: TypedProgramRequirement<'Course'> }> = ({ data }) => {
  const requiredAmount = data.courseCount === data.courses.length ? 'all' : data.courseCount;
  return (
    <div className="group-requirement">
      <GroupHeader title={data.label} />
      {data.courses.length > 1 && (
        <p>
          <b>Complete {requiredAmount} of the following:</b>
        </p>
      )}
      {/* COURSE1: <pre>{data.courses.join('\n')}</pre> */}
      <div className="group-courses">
        {data.courses.map((c) => (
          <CourseTile courseID={c} key={c} />
        ))}
      </div>
    </div>
  );
};

const GroupedCourseRequirement: FC<{ data: TypedProgramRequirement<'Course'> }> = ({ data }) => {
  return (
    <div className="course-requirement">
      {/* COURSE2: <pre>{data.courses.join('\n')}</pre> */}
      <div className="group-courses">
        {data.courses.map((c) => (
          <CourseTile courseID={c} key={c} />
        ))}
      </div>
    </div>
  );
};

const GroupRequirement: FC<{ data: TypedProgramRequirement<'Group'> }> = ({ data }) => {
  return (
    <div className="group-requirement">
      <GroupHeader title={data.label} />
      <p>
        Complete <b>{data.requirementCount}</b> of the following series:
      </p>
      {data.requirements.map((r, i) => (
        <React.Fragment key={i}>
          <p>
            <b>{r.label}</b>
          </p>
          <ProgramRequirementDisplay requirement={r} nested />
        </React.Fragment>
      ))}
    </div>
  );
};

interface ProgramRequirementDisplayProps {
  requirement: ProgramRequirement;
  nested?: boolean;
}
const ProgramRequirementDisplay: FC<ProgramRequirementDisplayProps> = ({ requirement, nested }) => {
  switch (requirement.requirementType) {
    case 'Course': {
      const DisplayComponent = nested ? GroupedCourseRequirement : CourseRequirement;
      return <DisplayComponent data={requirement} />;
    }
    case 'Group':
      return <GroupRequirement data={requirement} />;
    case 'Unit':
      return <></>;
  }
};

interface RequireCourseListProps {
  requirements: ProgramRequirement[];
}

const RequiredCourseList: FC<RequireCourseListProps> = ({ requirements }) => {
  const collapsedRequirements = collapseSingletonRequirements(requirements);

  return (
    <div className="required-courses">
      {/* Groups of courses or something */}
      {/* <pre><code>{JSON.stringify(collapsedRequirements, null, 2)}</code></pre> */}
      {/* key is ok because we don't reorder these */}
      {collapsedRequirements.map((r, i) => (
        <ProgramRequirementDisplay requirement={r} key={i} />
      ))}
    </div>
  );
};

export default RequiredCourseList;
