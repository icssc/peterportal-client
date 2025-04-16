import { FC, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';

interface ScoreSelectionProps {
  score: number | null;
  setScore: (value: number) => void;
}

const ScoreSelection: FC<ScoreSelectionProps> = ({ score, setScore }) => {
  /** @todo style this according to the Figma, add options & optgroup titled 'Score' */
  return (
    <select value={score ?? ''} onInput={(event) => setScore(parseInt((event.target as HTMLInputElement).value))}>
      <option>1</option>
    </select>
  );
};

const APCreditMenuTile: FC = () => {
  const [score, setScore] = useState<number | null>(null);
  const [units, setUnits] = useState<number>(0);

  const selectBox = <ScoreSelection score={score} setScore={setScore} />;
  const deleteFn = () => {};

  return (
    <MenuTile title="AP Sample Name" headerItems={selectBox} units={units} setUnits={setUnits} deleteFn={deleteFn}>
      <p>A list of courses cleared by this exam</p>
    </MenuTile>
  );
};

const APExamsSection: FC = () => {
  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      {/** @todo map each user AP Exam to one of these tiles */}
      <APCreditMenuTile />
    </MenuSection>
  );
};

export default APExamsSection;
