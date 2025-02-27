import { FC, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import trpc from '../../../trpc';
import RequirementsLoadingIcon from './RequirementsLoadingIcon';
import { setGERequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

function getCoursesForGE() {
  return trpc.programs.getRequiredCoursesUgrad.query({ id: 'GE' });
}

const GERequiredCourseList: FC = () => {
  const requirements = useAppSelector((state) => state.courseRequirements.geRequirements);

  const [resultsLoading, setResultsLoading] = useState(false);

  const dispatch = useAppDispatch();

  // Initially fetch requirements
  useEffect(() => {
    if (requirements.length) return;

    setResultsLoading(true);
    getCoursesForGE().then((geReqs) => {
      dispatch(setGERequirements(geReqs));
      setResultsLoading(false);
    });
  }, [dispatch, requirements]);

  return <>{resultsLoading ? <RequirementsLoadingIcon /> : <ProgramRequirementsList requirements={requirements} />}</>;
};

export default GERequiredCourseList;
