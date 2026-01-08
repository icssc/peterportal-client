import { FC, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import trpc from '../../../trpc';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import { setGERequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { normalizeTitleLabels } from '../../../helpers/substitutions';
import { Select, MenuItem, Divider } from '@mui/material';

async function getCoursesForGE() {
  const fetchedRequirements = await trpc.programs.getRequiredCoursesUgrad.query({ id: 'GE' });
  normalizeTitleLabels(fetchedRequirements);

  return fetchedRequirements;
}

const chcRequirements = () => {
  return (
    <div>
      <Select fullWidth displayEmpty defaultValue={''} className="ppc-combobox">
        <MenuItem key="" value="">
          Not Enrolled in Campuswide Honors
        </MenuItem>
        <MenuItem key="CHC4" value="CHC4">
          4-Year CHC Student
        </MenuItem>
        <MenuItem key="CHC2" value="CHC2">
          2-Year CHC Student
        </MenuItem>
      </Select>
    </div>
  );
};
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

  if (resultsLoading) return <LoadingSpinner />;

  return (
    <div className="program-requirements">
      <ProgramRequirementsList requirements={requirements} storeKeyPrefix="ge" />
      <Divider />
      {chcRequirements()}
    </div>
  );
};

export default GERequiredCourseList;
