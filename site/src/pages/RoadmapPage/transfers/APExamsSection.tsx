import { FC, useState, useContext, useEffect, useCallback } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import Select from 'react-select';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { addUserAPExam, removeUserAPExam, updateUserExam } from '../../../store/slices/transferCreditsSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import { APExam, TransferredAPExam } from '@peterportal/types';
import './APExamsSection.scss';

interface ScoreSelectionProps {
  score: number;
  setScore: (value: number) => void;
}

interface APExamOption {
  value: APExam;
  label: string;
}

type CoursesGrantedTree = string | { AND: CoursesGrantedTree[] } | { OR: CoursesGrantedTree[] };

function formatCourses(tree: CoursesGrantedTree): string {
  if (typeof tree === 'string') return tree;

  const isAnd = 'AND' in tree;
  const children = isAnd ? tree.AND : (tree as { OR: CoursesGrantedTree[] }).OR;
  const connector = isAnd ? ', ' : ' or ';

  const formatted = children.map(formatCourses);

  return formatted.join(connector);
}

const ScoreSelection: FC<ScoreSelectionProps> = ({ score, setScore }) => {
  return (
    <div className="select">
      <select
        value={score ?? ''}
        onInput={(event) => setScore(parseInt((event.target as HTMLInputElement).value))}
        className="select-box"
      >
        <optgroup label="Score">
          <option>1</option>
          <option>2</option>
          <option>3</option>
          <option>4</option>
          <option>5</option>
        </optgroup>
      </select>
    </div>
  );
};

interface APCreditMenuTileProps {
  userExamInfo: TransferredAPExam;
}

const APCreditMenuTile: FC<APCreditMenuTileProps> = ({ userExamInfo }) => {
  const { examName, score, units } = userExamInfo;
  const updateScore = (value: number) => handleUpdate(value, units);
  const updateUnits = (value: number) => handleUpdate(score, value);

  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();

  const selectBox = <ScoreSelection score={score} setScore={updateScore} />;

  const handleUpdate = useCallback(
    async (newScore: number, newUnits: number) => {
      dispatch(updateUserExam({ examName, score: newScore, units: newUnits }));
      if (!isLoggedIn) return;
      trpc.transferCredits.updateUserAPExam.mutate({ examName, score: newScore, units: newUnits });
    },
    [dispatch, examName, isLoggedIn],
  );

  const deleteFn = useCallback(() => {
    dispatch(removeUserAPExam(examName));
    if (!isLoggedIn) return;
    trpc.transferCredits.deleteUserAPExam.mutate(examName);
  }, [dispatch, examName, isLoggedIn]);

  const apiExamInfo = apExamInfo.find((exam) => exam.fullName === examName);
  let message = '';

  for (const reward of apiExamInfo?.rewards ?? []) {
    if (!reward.acceptableScores.includes(score)) continue;
    const { coursesGranted } = reward;
    const formatted = formatCourses(coursesGranted as CoursesGrantedTree);
    if (formatted) message += `${message ? '\n' : ''}${formatted}`;
    break;
  }

  return (
    <MenuTile title={examName} headerItems={selectBox} units={units} setUnits={updateUnits} deleteFn={deleteFn}>
      <p>{message ? 'Clears ' + message : 'This exam does not clear any courses'}</p>
    </MenuTile>
  );
};

const APExamsSection: FC = () => {
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();
  const isDark = useContext(ThemeContext).darkMode;
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const userAPExams = useAppSelector((state) => state.transferCredits.userAPExams);
  const [examName, setExamName] = useState<string | null>(null);
  const [score, setScore] = useState<number | null>(null);

  // Save AP Exam to store
  useEffect(() => {
    if (!examName || !score) return;
    const examInfo = apExamInfo.find((exam) => exam.fullName === examName);
    const units = examInfo?.rewards?.find((r) => r.acceptableScores.includes(score))?.unitsGranted ?? 0;

    if (!userAPExams.find((exam) => exam.examName === examName)) {
      dispatch(addUserAPExam({ examName, score, units }));
      if (isLoggedIn) trpc.transferCredits.addUserAPExam.mutate({ examName, score, units });
    }
    // Remove exam from select options
    setExamName(null);
    setScore(null);
  }, [dispatch, examName, score, apExamInfo, userAPExams, isLoggedIn]);

  const baseSelectOptions: APExamOption[] = apExamInfo.map((exam) => ({
    value: exam,
    label: exam.fullName,
  }));

  const apSelectOptions = baseSelectOptions.filter((exam) => {
    return !userAPExams.some((userExam) => userExam.examName === exam.label);
  });

  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      {userAPExams.map((exam) => (
        <APCreditMenuTile key={exam.examName} userExamInfo={exam} />
      ))}
      <div className="ap-import-row">
        <div className="exam-input">
          <Select
            className="ppc-combobox"
            classNamePrefix="ppc-combobox"
            value={apSelectOptions.find((opt) => opt.label === examName) ?? null}
            options={apSelectOptions}
            isSearchable
            onChange={(selectedOption) => setExamName(selectedOption?.label || null)}
            placeholder="Add an AP Exam..."
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
        <div className="score-input">
          <Select
            className="ppc-combobox"
            classNamePrefix="ppc-combobox"
            value={score ? { value: score, label: score.toString() } : null}
            options={[
              { value: 1, label: '1' },
              { value: 2, label: '2' },
              { value: 3, label: '3' },
              { value: 4, label: '4' },
              { value: 5, label: '5' },
            ]}
            onChange={(selectedOption) => setScore(selectedOption?.value || null)}
            placeholder="Score"
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
      </div>
    </MenuSection>
  );
};

export default APExamsSection;
