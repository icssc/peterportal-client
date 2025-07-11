import { FC, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import trpc from '../../../trpc';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import { setGERequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { normalizeTitleLabels } from '../../../helpers/substitutions';

async function getCoursesForGE() {
  const fetchedRequirements = await trpc.programs.getRequiredCoursesUgrad.query({ id: 'GE' });
  normalizeTitleLabels(fetchedRequirements);

  return fetchedRequirements;
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

  return (
    <>
      {resultsLoading ? (
        <LoadingSpinner />
      ) : (
        <ProgramRequirementsList requirements={requirements} storeKeyPrefix="ge" />
      )}
    </>
  );
};

export default GERequiredCourseList;
