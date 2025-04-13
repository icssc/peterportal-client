import './MajorCourseList.scss';
import { FC, useCallback, useEffect, useState } from 'react';
import { ChevronDown, ChevronRight } from 'react-bootstrap-icons';
import ProgramRequirementsList from './ProgramRequirementsList';
import { setMinorRequirements, minorRequirements } from '../../../store/slices/courseRequirementsSlice';
import RequirementsLoadingIcon from './RequirementsLoadingIcon';
import trpc from '../../../trpc';
import { useAppDispatch } from '../../../store/hooks';

function getCoursesForMinor(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'minor', programId });
}

interface MinorCourseListProps {
  minorReqs: minorRequirements;
}

const MinorCourseList: FC<MinorCourseListProps> = ({ minorReqs }) => {
  const [resultsLoading, setResultsLoading] = useState(false);
  const [open, setOpen] = useState(true);

  const dispatch = useAppDispatch();

  const fetchRequirements = useCallback(
    async (minorId: string) => {
      setResultsLoading(true);

      try {
        const requirements = await getCoursesForMinor(minorId);
        dispatch(setMinorRequirements({ minorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch],
  );

  useEffect(() => {
    fetchRequirements(minorReqs.minor.id);
  }, [fetchRequirements, minorReqs.minor.id]);

  const toggleExpand = () => setOpen(!open);

  const renderRequirements = () => {
    if (resultsLoading) return <RequirementsLoadingIcon />;
    return <ProgramRequirementsList requirements={minorReqs.requirements} />;
  };

  return (
    <div className="major-section">
      <button className="header-tab" onClick={toggleExpand}>
        {open ? <ChevronDown size={18} /> : <ChevronRight size={18} />}
        <h4 className="major-name">{minorReqs.minor.name}</h4>
      </button>
      {open && <>{renderRequirements()}</>}
    </div>
  );
};

export default MinorCourseList;
