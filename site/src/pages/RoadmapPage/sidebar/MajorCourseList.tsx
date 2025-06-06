import { FC, useCallback, useContext, useEffect, useState } from 'react';
import Select from 'react-select';

import './MajorCourseList.scss';
import ProgramRequirementsList from './ProgramRequirementsList';
import { MajorSpecialization } from '@peterportal/types';

import trpc from '../../../trpc';
import ThemeContext from '../../../style/theme-context';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import {
  MajorWithSpecialization,
  setMajorSpecs,
  setRequirements,
  setSpecialization,
} from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch } from '../../../store/hooks';
import { comboboxTheme, normalizeMajorName } from '../../../helpers/courseRequirements';

import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';

interface MajorCourseListProps {
  majorWithSpec: MajorWithSpecialization;
  onSpecializationChange: (majorId: string, spec: MajorSpecialization | null) => void;
  selectedSpecId?: string;
}
const MajorCourseList: FC<MajorCourseListProps> = ({ majorWithSpec, onSpecializationChange, selectedSpecId }) => {
  const isDark = useContext(ThemeContext).darkMode;
  const [specsLoading, setSpecsLoading] = useState(false);
  const [resultsLoading, setResultsLoading] = useState(false);
  const [open, setOpen] = useState(true);

  const { major, selectedSpec, specializations } = majorWithSpec;
  const hasSpecs = major.specializations.length > 0;
  const specOptions = specializations.map((s) => ({ value: s, label: s.name }));

  const dispatch = useAppDispatch();

  const loadSpecs = useCallback(async () => {
    setSpecsLoading(true);
    try {
      const specs = await trpc.programs.getSpecializations.query({ major: major.id });
      specs.forEach((s) => (s.name = normalizeMajorName(s)));
      specs.sort((a, b) => a.name.localeCompare(b.name));
      dispatch(setMajorSpecs({ majorId: major.id, specializations: specs }));
    } finally {
      setSpecsLoading(false);
    }
  }, [dispatch, major.id]);

  const fetchRequirements = useCallback(
    async (majorId: string, specId?: string | null) => {
      setResultsLoading(true);
      try {
        const requirements = await trpc.programs.getRequiredCourses.query({ type: 'major', programId: majorId });
        if (specId) {
          const specRequirements = await trpc.programs.getRequiredCourses.query({
            type: 'specialization',
            programId: specId,
          });
          requirements.push(...specRequirements);
        }
        dispatch(setRequirements({ majorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch],
  );

  const loadSpecRequirements = useCallback(async () => {
    if (!hasSpecs) {
      if (majorWithSpec.requirements.length === 0) {
        await fetchRequirements(major.id, null);
      }
      return;
    }

    if (!selectedSpecId && !selectedSpec?.id) return;
    if (selectedSpecId === selectedSpec?.id) return;

    const specs = await trpc.programs.getSpecializations.query({ major: major.id });
    const foundSpec = specs.find((s) => s.id === selectedSpecId);
    if (foundSpec) {
      dispatch(setSpecialization({ majorId: major.id, specialization: foundSpec }));
      await fetchRequirements(major.id, selectedSpec?.id);
    }
  }, [
    dispatch,
    fetchRequirements,
    hasSpecs,
    major.id,
    majorWithSpec.requirements.length,
    selectedSpecId,
    selectedSpec?.id,
  ]);

  // Initial Loader
  useEffect(() => {
    if (specOptions.length) return;
    const loadSpecsAndSpecRequirements = async () => {
      if (hasSpecs) await loadSpecs();
      await loadSpecRequirements();
    };
    loadSpecsAndSpecRequirements();
  }, [hasSpecs, loadSpecRequirements, specOptions.length, loadSpecs]);

  const handleSpecializationChange = useCallback(
    async (data: { value: MajorSpecialization; label: string } | null) => {
      const updatedSpec = data?.value || null;
      if (updatedSpec?.id === selectedSpecId) return;
      setResultsLoading(true);
      onSpecializationChange(major.id, data?.value ?? null);
      dispatch(setRequirements({ majorId: major.id, requirements: [] }));
      dispatch(setSpecialization({ majorId: major.id, specialization: updatedSpec }));
      await fetchRequirements(major.id, updatedSpec?.id || null);
    },
    [dispatch, fetchRequirements, major, onSpecializationChange, selectedSpecId],
  );

  const toggleExpand = () => setOpen(!open);

  return (
    <div className="major-section">
      <button className="header-tab" onClick={toggleExpand}>
        {open ? <ExpandMoreIcon /> : <ChevronRightIcon />}
        <h4 className="major-name">{major.name}</h4>
      </button>
      {open && (
        <>
          {hasSpecs && (
            <Select
              options={specOptions}
              value={specOptions.find((s) => s.value.id === (majorWithSpec.selectedSpec?.id ?? selectedSpecId)) ?? null}
              isDisabled={specsLoading}
              isLoading={specsLoading}
              onChange={handleSpecializationChange}
              className="ppc-combobox"
              classNamePrefix="ppc-combobox"
              placeholder="Select a specialization..."
              theme={(t) => comboboxTheme(t, isDark)}
            />
          )}
          {resultsLoading ? (
            <LoadingSpinner />
          ) : hasSpecs && !majorWithSpec.selectedSpec ? (
            <p>Please select a specialization to view requirements.</p>
          ) : (
            <ProgramRequirementsList
              requirements={majorWithSpec.requirements}
              storeKeyPrefix={`major-${majorWithSpec.major.id}`}
            />
          )}
        </>
      )}
    </div>
  );
};

export default MajorCourseList;
