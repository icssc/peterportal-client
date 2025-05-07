import './MajorCourseList.scss';
import { FC, useCallback, useContext, useEffect, useState } from 'react';
import Select from 'react-select';
import { ChevronDown, ChevronRight } from 'react-bootstrap-icons';
import ProgramRequirementsList from './ProgramRequirementsList';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme, normalizeMajorName } from '../../../helpers/courseRequirements';
import {
  MajorWithSpecialization,
  setMajorSpecs,
  setRequirements,
  setSpecialization,
} from '../../../store/slices/courseRequirementsSlice';
import { MajorSpecialization } from '@peterportal/types';
import RequirementsLoadingIcon from './RequirementsLoadingIcon';
import trpc from '../../../trpc';
import { useAppDispatch } from '../../../store/hooks';

function getMajorSpecializations(majorId: string) {
  return trpc.programs.getSpecializations.query({ major: majorId });
}

function getCoursesForMajor(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'major', programId });
}

function getCoursesForSpecialization(programId?: string | null) {
  if (!programId) return [];
  return trpc.programs.getRequiredCourses.query({ type: 'specialization', programId });
}

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

  const { major, selectedSpec: specialization, specializations } = majorWithSpec;
  const hasSpecs = major.specializations.length > 0;
  const specOptions = specializations.map((s) => ({ value: s, label: s.name }));

  const dispatch = useAppDispatch();

  const loadSpecs = useCallback(async () => {
    setSpecsLoading(true);
    try {
      const specs = await getMajorSpecializations(major.id);
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
        const requirements = await getCoursesForMajor(majorId);
        requirements.push(...(await getCoursesForSpecialization(specId)));
        dispatch(setRequirements({ majorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch],
  );

  const loadSpecRequirements = useCallback(async () => {
    if (!hasSpecs) {
      if (majorWithSpec.requirements.length > 0) return;
      else return await fetchRequirements(major.id, null);
    }
    if (!selectedSpecId && !specialization?.id) return;
    if (selectedSpecId === specialization?.id) return;

    const specs = await getMajorSpecializations(major.id);
    const foundSpec = specs.find((s) => s.id === selectedSpecId);
    if (foundSpec) {
      dispatch(setSpecialization({ majorId: major.id, specialization: foundSpec }));
      await fetchRequirements(major.id, specialization?.id);
    }
  }, [
    dispatch,
    fetchRequirements,
    hasSpecs,
    major.id,
    majorWithSpec.requirements.length,
    selectedSpecId,
    specialization?.id,
  ]);

  // Initial Loader
  useEffect(() => {
    if (specOptions.length) return;
    if (hasSpecs && !specOptions.length) {
      loadSpecs().then(loadSpecRequirements);
    } else {
      loadSpecRequirements();
    }
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

  const renderRequirements = () => {
    if (resultsLoading) return <RequirementsLoadingIcon />;
    if (hasSpecs && !majorWithSpec.selectedSpec) {
      return <p className="unselected-spec-notice">Please select a specialization to view requirements</p>;
    }
    return (
      <ProgramRequirementsList
        requirements={majorWithSpec.requirements}
        storeKeyPrefix={`major-${majorWithSpec.major.id}`}
      />
    );
  };

  return (
    <div className="major-section">
      <button className="header-tab" onClick={toggleExpand}>
        {open ? <ChevronDown size={18} /> : <ChevronRight size={18} />}
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
          {renderRequirements()}
        </>
      )}
    </div>
  );
};

export default MajorCourseList;
