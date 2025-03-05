import { FC, useCallback, useContext, useEffect, useState } from 'react';
import Select from 'react-select';
import { ChevronDown, ChevronRight } from 'react-bootstrap-icons';
import ProgramRequirementsList from './ProgramRequirementsList';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme, normalizeMajorName } from '../../../helpers/courseRequirements';
import {
  MajorWithSpecialization,
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

interface SingleMajorCourseListProps {
  majorWithSpec: MajorWithSpecialization;
  isExpanded: boolean;
  onToggleExpand: (majorId: string) => void;
  onSpecializationChange: (majorId: string, spec: MajorSpecialization | null) => void;
  selectedSpecId?: string;
}

const SingleMajorCourseList: FC<SingleMajorCourseListProps> = ({
  majorWithSpec,
  isExpanded,
  onToggleExpand,
  onSpecializationChange,
  selectedSpecId,
}) => {
  const isDark = useContext(ThemeContext).darkMode;
  const [specsLoading, setSpecsLoading] = useState(false);
  const [specOptions, setSpecOptions] = useState<{ value: MajorSpecialization; label: string }[]>([]);
  const [resultsLoading, setResultsLoading] = useState(false);
  const { major, specialization } = majorWithSpec;
  const hasSpecs = major.specializations.length > 0;

  const dispatch = useAppDispatch();

  // DATA LOADERS

  const updateAvailableSpecs = useCallback(async () => {
    setSpecsLoading(true);

    try {
      const specs = await getMajorSpecializations(major.id);
      specs.forEach((s) => (s.name = normalizeMajorName(s)));
      specs.sort((a, b) => a.name.localeCompare(b.name));
      const options = specs.map((s) => ({ value: s, label: s.name }));
      setSpecOptions(options);
    } finally {
      setSpecsLoading(false);
    }
  }, [major.id]);

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
    console.log(hasSpecs, major.id);
    if (!hasSpecs) return await fetchRequirements(major.id, null);
    if (!selectedSpecId && !specialization?.id) return;
    if (selectedSpecId === specialization?.id) return;

    const specs = await getMajorSpecializations(major.id);
    const foundSpec = specs.find((s) => s.id === selectedSpecId);
    if (foundSpec) {
      dispatch(setSpecialization({ majorId: major.id, specialization: foundSpec }));
      await fetchRequirements(major.id, specialization?.id);
    }
  }, [dispatch, fetchRequirements, hasSpecs, major.id, selectedSpecId, specialization]);

  // Initial Loader
  useEffect(() => {
    if (specOptions.length) return;
    if (hasSpecs && !specOptions.length) {
      updateAvailableSpecs().then(loadSpecRequirements);
    } else {
      loadSpecRequirements();
    }
  }, [hasSpecs, loadSpecRequirements, specOptions.length, updateAvailableSpecs]);

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

  const handleToggleExpand = () => {
    onToggleExpand(major.id);
  };

  const renderRequirements = () => {
    if (resultsLoading) return <RequirementsLoadingIcon />;
    if (hasSpecs && !majorWithSpec.specialization) {
      return <div className="mt-3 text-muted">Please select a specialization to view requirements</div>;
    }
    return <ProgramRequirementsList requirements={majorWithSpec.requirements} />;
  };

  return (
    <div className="major-section mt-3">
      <div
        role="button"
        tabIndex={0}
        className="d-flex align-items-center cursor-pointer"
        onClick={handleToggleExpand}
        onKeyDown={(e) => {
          if (e.key === 'Enter' || e.key === ' ') {
            e.preventDefault();
            handleToggleExpand();
          }
        }}
      >
        <h4 className="mb-0 flex-grow-1">{major.name}</h4>
        <span className="ms-2">{isExpanded ? <ChevronDown size={18} /> : <ChevronRight size={18} />}</span>
      </div>
      {isExpanded && (
        <>
          {hasSpecs && (
            <Select
              options={specOptions}
              // value={specOptions.find(s => s.value.id === majorWithSpec.specialization?.id) ?? null}
              value={
                specOptions.find((s) => s.value.id === (majorWithSpec.specialization?.id ?? selectedSpecId)) ?? null
              }
              isDisabled={specsLoading}
              isLoading={specsLoading}
              onChange={handleSpecializationChange}
              className="ppc-combobox mt-3"
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

export default SingleMajorCourseList;
