import { FC, useEffect, useState } from 'react';
import ProgramRequirementsList from './RequiredCourseList';
import Select from 'react-select';
import trpc from '../../../trpc';
import { MajorProgram, MajorSpecialization, ProgramRequirement } from '@peterportal/types';
import { normalizeMajorName } from '../../../helpers/courseRequirements';
import { Spinner } from 'react-bootstrap';

const MajorRequiredCourseList: FC = () => {
  const [majors, setMajors] = useState<MajorProgram[]>([]);
  const [selectedMajor, setSelectedMajor] = useState<MajorProgram | null>(null);
  const [specializations, setSpecializations] = useState<MajorSpecialization[]>([]);
  const [selectedSpec, setSelectedSpec] = useState<MajorSpecialization | null>(null);

  const [requirements, setRequirements] = useState<ProgramRequirement[]>([]);
  const [specsLoading, setSpecsLoading] = useState(false);
  const [resultsLoading, setResultsLoading] = useState(false);

  // Initial Load, fetch majors
  useEffect(() => {
    trpc.programs.getMajors.query().then((majors) => {
      majors.forEach((m) => {
        m.name = normalizeMajorName(m);
      });
      majors.sort((a, b) => a.name.localeCompare(b.name));
      setMajors(majors);
    });
  }, []);

  // Major with specs selected, fetch specializations
  useEffect(() => {
    if (!selectedMajor?.id) return;

    setSpecializations([]);
    setSelectedSpec(null);
    if (!selectedMajor.specializations.length) return;

    setSpecsLoading(true);
    trpc.programs.getSpecializations.query({ major: selectedMajor.id }).then((specs) => {
      specs.forEach((s) => {
        s.name = normalizeMajorName(s);
      });
      specs.sort((a, b) => a.name.localeCompare(b.name));
      setSpecializations(specs);
      setSpecsLoading(false);
    });
  }, [selectedMajor]);

  // Spec or Major without specs selected, fetch requirements
  useEffect(() => {
    if (!selectedMajor?.id) return;
    if (selectedMajor.specializations.length && !selectedSpec) return;

    setResultsLoading(true);
    trpc.programs.getRequiredCourses.query({ type: 'major', programId: selectedMajor.id }).then(async (majorReqs) => {
      if (!selectedSpec) {
        setRequirements(majorReqs);
        setResultsLoading(false);
        return;
      }
      const specReqs = await trpc.programs.getRequiredCourses.query({
        type: 'specialization',
        programId: selectedSpec.id,
      });
      setRequirements(majorReqs.concat(specReqs));
      setResultsLoading(false);
    });
  }, [selectedMajor, selectedSpec]);

  const majorSelectOptions = majors.map((m) => ({
    value: m,
    label: `${m.name}, ${m.type}`,
  }));

  const specSelectOptions = specializations.map((s) => ({ value: s, label: s.name }));

  const selectBoxClassNames = {
    container: () => 'ppc-combobox',
    menu: () => 'ppc-combobox-menu',
    control: () => 'ppc-combobox-control',
  };

  const loadingIcon = (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );

  return (
    <>
      <Select
        options={majorSelectOptions}
        isDisabled={majors.length === 0}
        isLoading={majors.length === 0}
        onChange={(data) => setSelectedMajor(data!.value)}
        classNames={selectBoxClassNames}
        placeholder="Select a major..."
      />
      {selectedMajor && !!selectedMajor.specializations.length && (
        <Select
          options={specSelectOptions}
          key={selectedMajor.id} // force re-render on changing major
          isDisabled={specsLoading}
          isLoading={specsLoading}
          onChange={(data) => setSelectedSpec(data!.value)}
          classNames={selectBoxClassNames}
          placeholder="Select a specialization..."
        />
      )}
      {selectedMajor && (!selectedMajor.specializations.length || selectedSpec) && (
        <>{resultsLoading ? loadingIcon : <ProgramRequirementsList requirements={requirements} />}</>
      )}
    </>
  );
};

export default MajorRequiredCourseList;
