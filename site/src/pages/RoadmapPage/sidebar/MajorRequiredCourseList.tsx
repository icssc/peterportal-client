import { FC, useContext, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import Select from 'react-select';
import trpc from '../../../trpc';
import { normalizeMajorName, comboboxTheme } from '../../../helpers/courseRequirements';
import { Spinner } from 'react-bootstrap';
import {
  setMajor,
  setMajorList,
  setRequirements,
  setSpecialization,
  setSpecializationList,
} from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import ThemeContext from '../../../style/theme-context';

const MajorRequiredCourseList: FC = () => {
  const isDark = useContext(ThemeContext).darkMode;
  const majors = useAppSelector((state) => state.courseRequirements.majorList);
  const specializations = useAppSelector((state) => state.courseRequirements.specializationList);
  const selectedMajor = useAppSelector((state) => state.courseRequirements.major);
  const selectedSpec = useAppSelector((state) => state.courseRequirements.specialization);

  /** Must ONLY contain requirements for the selected major/spec. This is used to check whether a major is
   * already loaded, so it must be set to empty if we change the major or spec */
  const requirements = useAppSelector((state) => state.courseRequirements.currentRequirements);

  const [specsLoading, setSpecsLoading] = useState(false);
  const [resultsLoading, setResultsLoading] = useState(false);

  const dispatch = useAppDispatch();

  // Initial Load, fetch majors
  useEffect(() => {
    if (majors.length) return;
    trpc.programs.getMajors.query().then((majors) => {
      majors.forEach((m) => {
        m.name = normalizeMajorName(m);
      });
      majors.sort((a, b) => a.name.localeCompare(b.name));
      dispatch(setMajorList(majors));
    });
  }, [dispatch, majors.length]);

  // Major with specs selected, fetch specializations
  useEffect(() => {
    if (!selectedMajor?.id) return;
    if (selectedSpec) return;
    if (requirements.length) return;

    dispatch(setSpecializationList([]));
    if (!selectedMajor.specializations.length) return;

    setSpecsLoading(true);
    trpc.programs.getSpecializations.query({ major: selectedMajor.id }).then((specs) => {
      specs.forEach((s) => {
        s.name = normalizeMajorName(s);
      });
      specs.sort((a, b) => a.name.localeCompare(b.name));
      dispatch(setSpecializationList(specs));
      setSpecsLoading(false);
    });
  }, [dispatch, selectedMajor, selectedSpec, requirements.length]);

  // Spec or Major without specs selected, fetch requirements
  useEffect(() => {
    if (!selectedMajor?.id) return;
    if (selectedMajor.specializations.length && !selectedSpec) return;
    if (requirements.length) return;

    setResultsLoading(true);
    trpc.programs.getRequiredCourses.query({ type: 'major', programId: selectedMajor.id }).then(async (majorReqs) => {
      if (!selectedSpec) {
        dispatch(setRequirements(majorReqs));
        setResultsLoading(false);
        return;
      }
      const specReqs = await trpc.programs.getRequiredCourses.query({
        type: 'specialization',
        programId: selectedSpec.id,
      });
      dispatch(setRequirements(majorReqs.concat(specReqs)));
      setResultsLoading(false);
    });
  }, [dispatch, selectedMajor, selectedSpec, requirements.length]);

  const majorSelectOptions = majors.map((m) => ({
    value: m,
    label: `${m.name}, ${m.type}`,
  }));

  const specSelectOptions = specializations.map((s) => ({ value: s, label: s.name }));

  const loadingIcon = (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );

  return (
    <>
      <Select
        options={majorSelectOptions}
        defaultValue={majorSelectOptions.find((o) => o.value === selectedMajor)}
        isDisabled={majors.length === 0}
        isLoading={majors.length === 0}
        onChange={(data) => {
          if (data!.value.id === selectedMajor?.id) return;
          dispatch(setRequirements([])); // set to empty immediately because otherwise it's out of date
          dispatch(setMajor(data!.value));
          dispatch(setSpecialization(null));
        }}
        className="ppc-combobox"
        classNamePrefix="ppc-combobox"
        placeholder="Select a major..."
        theme={(t) => comboboxTheme(t, isDark)}
      />
      {selectedMajor && !!selectedMajor.specializations.length && (
        <Select
          options={specSelectOptions}
          defaultValue={specSelectOptions.find((o) => o.value === selectedSpec)}
          key={selectedMajor.id} // force re-render on changing major
          isDisabled={specsLoading}
          isLoading={specsLoading}
          onChange={(data) => {
            if (data!.value.id === selectedSpec?.id) return;
            setResultsLoading(true);
            dispatch(setRequirements([])); // set to empty immediately because otherwise it's out of date
            dispatch(setSpecialization(data!.value));
          }}
          className="ppc-combobox"
          classNamePrefix="ppc-combobox"
          placeholder="Select a specialization..."
          theme={(t) => comboboxTheme(t, isDark)}
        />
      )}
      {selectedMajor && (!selectedMajor.specializations.length || selectedSpec) && (
        <>{resultsLoading ? loadingIcon : <ProgramRequirementsList requirements={requirements} />}</>
      )}
    </>
  );
};

export default MajorRequiredCourseList;
