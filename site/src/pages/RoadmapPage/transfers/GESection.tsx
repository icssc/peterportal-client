import './GESection.scss';
import { FC, FormEvent, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';

const GETitles = [
  'I. Lower-Division Writing',
  'I. Upper-Division Writing',
  'II. Science and Technology',
  'III. Social and Behavioral Sciences',
  'IV. Arts and Humanities',
  'Va. Quantitative Literacy',
  'Vb. Formal Reasoning',
  'VI. Language Other Than English',
  'VII. Multicultural Studies',
  'VIII. International/Global Issues',
];

interface GEInputProps {
  value: number;
  setValue: (value: number) => void;
}

const GEInput: FC<GEInputProps> = ({ value, setValue }) => {
  const handleSubmit = (event: FormEvent) => {
    event.preventDefault();
    const formData = new FormData(event.target as HTMLFormElement);
    const value = parseFloat(formData.get('value') as string);
    setValue(value);
  };

  return (
    <form onSubmit={handleSubmit}>
      {/** @todo add restriction for integers only, no floats */}
      <input className="ge-input" type="number" min="0" name="value" defaultValue={value} />
    </form>
  );
};

interface GETileProps {
  title: string;
}

const GETile: FC<GETileProps> = ({ title }) => {
  {
    /** @todo figure out how to save this data */
  }
  const [numCourses, setNumCourses] = useState<number>(0);
  const [units, setUnits] = useState<number>(0);

  return (
    <MenuTile title={title}>
      <div className="ge-inputs">
        <div className="ge-input-container">
          <p>Number of Courses:</p>
          <GEInput value={numCourses} setValue={setNumCourses} />
        </div>
        <div className="ge-input-container">
          <p>Units Transferred:</p>
          <GEInput value={units} setValue={setUnits} />
        </div>
      </div>
    </MenuTile>
  );
};

const GESection: FC = () => {
  return (
    <MenuSection title="General Education Credits">
      <SectionDescription>GE Section Description</SectionDescription>
      {GETitles.map((title) => (
        <GETile key={title} title={title} />
      ))}
    </MenuSection>
  );
};

export default GESection;
