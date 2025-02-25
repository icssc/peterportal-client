import { FC, useCallback, useContext } from 'react';
import Select from 'react-select';
import { Spinner } from 'react-bootstrap';
import { ChevronDown, ChevronRight } from 'react-bootstrap-icons';
import ProgramRequirementsList from './ProgramRequirementsList';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import { MajorWithSpecialization } from '../../../store/slices/courseRequirementsSlice';
import { MajorSpecialization } from '@peterportal/types';

interface SingleMajorCourseListProps {
  majorWithSpec: MajorWithSpecialization;
  isExpanded: boolean;
  onToggleExpand: (majorId: string) => void;
  specOptions: { value: MajorSpecialization; label: string }[];
  specsLoading: boolean;
  resultsLoading: boolean;
  onSpecializationChange: (majorId: string, data: { value: MajorSpecialization; label: string } | null) => void;
  fetchRequirements: (majorId: string, specializationId?: string | null) => Promise<void>;
}

const SingleMajorCourseList: FC<SingleMajorCourseListProps> = ({
  majorWithSpec,
  isExpanded,
  onToggleExpand,
  specOptions,
  specsLoading,
  resultsLoading,
  onSpecializationChange,
}) => {
  const isDark = useContext(ThemeContext).darkMode;

  const handleSpecializationChange = useCallback(
    async (data: { value: MajorSpecialization; label: string } | null) => {
      onSpecializationChange(majorWithSpec.major.id, data);
    },
    [majorWithSpec.major.id, onSpecializationChange],
  );

  const handleToggleExpand = () => {
    onToggleExpand(majorWithSpec.major.id);
  };

  const loadingIcon = (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );

  const renderRequirements = () => {
    if (resultsLoading) {
      return loadingIcon;
    } else if (majorWithSpec.major.specializations.length > 0 && !majorWithSpec.specialization) {
      return <div className="mt-3 text-muted">Please select a specialization to view requirements</div>;
    } else {
      return <ProgramRequirementsList requirements={majorWithSpec.requirements} />;
    }
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
        <h4 className="mb-0 flex-grow-1">{majorWithSpec.major.name}</h4>
        <span className="ms-2">{isExpanded ? <ChevronDown size={18} /> : <ChevronRight size={18} />}</span>
      </div>
      {isExpanded && (
        <>
          {majorWithSpec.major.specializations.length > 0 && (
            <Select
              options={specOptions || []}
              value={
                majorWithSpec.specialization
                  ? {
                      value: majorWithSpec.specialization,
                      label: majorWithSpec.specialization.name,
                    }
                  : null
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
