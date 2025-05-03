import { FC, useState, useContext, useEffect, useCallback } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import Select from 'react-select';
import ThemeContext from '../../../style/theme-context';
import { comboboxTheme } from '../../../helpers/courseRequirements';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setAPExams, addUserAPExam, removeUserAPExam } from '../../../store/slices/transferCreditsSlice';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import { APExams } from '@peterportal/types';
import './APExamsSection.scss';

type APExam = APExams[number];

interface ScoreSelectionProps {
  score: number;
  setScore: (value: number) => void;
}

interface APExamOption {
  value: APExam;
  label: string;
}

interface APCreditMenuTileProps {
  examName: string;
  userScore: number;
  userUnits: number;
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

const APCreditMenuTile: FC<APCreditMenuTileProps> = ({ examName, userScore, userUnits }) => {
  const [score, setScore] = useState<number>(userScore);
  const [units, setUnits] = useState<number>(userUnits);
  const APExams = useAppSelector((state) => state.transferCredits.APExams);
  const dispatch = useAppDispatch();

  const selectBox = <ScoreSelection score={score} setScore={setScore} />;

  const handleChange = useCallback(
    async (score: number, units: number) => {
      dispatch(removeUserAPExam(examName));
      dispatch(addUserAPExam({ examName, score: score, units: units }));
    },
    [dispatch, examName],
  );

  useEffect(() => {
    setScore(userScore);
    setUnits(userUnits);
  }, [examName, userScore, userUnits]);

  useEffect(() => {
    if (score !== userScore || units !== userUnits) {
      handleChange(score, units);
    }
  }, [score, units, userScore, userUnits, handleChange]);

  const deleteFn = useCallback(() => {
    dispatch(removeUserAPExam(examName));
  }, [dispatch, examName]);

  const exam = APExams.find((exam) => exam.fullName === examName);
  let message = '';

  for (const reward of exam?.rewards ?? []) {
    if (reward.acceptableScores.includes(score)) {
      const { coursesGranted } = reward;
      const formatted = formatCourses(coursesGranted as CoursesGrantedTree);
      if (formatted) {
        message += `${message ? '\n' : ''}${formatted}`;
      }
    }
  }

  return (
    <MenuTile title={examName} headerItems={selectBox} units={units} setUnits={setUnits} deleteFn={deleteFn}>
      <p>Clears {message || 'no courses'}</p>
    </MenuTile>
  );
};

const APExamsSection: FC = () => {
  const isLoggedIn = useIsLoggedIn;
  const dispatch = useAppDispatch();
  const isDark = useContext(ThemeContext).darkMode;
  const APExams = useAppSelector((state) => state.transferCredits.APExams);
  const userAPExams = useAppSelector((state) => state.transferCredits.userAPExams);
  const [currentExam, setCurrentExam] = useState<string | null>(null);
  const [currentScore, setCurrentScore] = useState<number | null>(null);
  const [examsLoading, setExamsLoading] = useState(false);

  // Save initial list of all AP Exams
  const saveAllAPExams = useCallback(
    (APExams: APExam[]) => {
      setExamsLoading(false);
      dispatch(setAPExams(APExams));
    },
    [dispatch],
  );

  // First render, fetch all AP Exams
  useEffect(() => {
    if (APExams.length) return;
    trpc.transferCredits.getAPExamInfo.query().then((allExams) => {
      const processedExams = allExams.map((exam) => ({
        ...exam,
        catalogueName: exam.catalogueName ?? null,
      }));
      saveAllAPExams(processedExams);
    });
  }, [dispatch, APExams.length, saveAllAPExams]);

  // Save AP Exams for user
  useEffect(() => {
    if (!isLoggedIn || !examsLoading) return;
    trpc.transferCredits.saveSelectedAPExams.mutate({ apExams: userAPExams });
  }, [userAPExams, isLoggedIn, examsLoading]);

  // Save AP Exam to store
  useEffect(() => {
    if (!currentExam || !currentScore) return;
    const foundUnits = APExams.find((exam) => exam.fullName === currentExam)?.rewards[0].unitsGranted ?? 0;
    dispatch(addUserAPExam({ examName: currentExam, score: currentScore, units: foundUnits }));
    // saveUserAPExams();
    setCurrentExam(null);
    setCurrentScore(null);
  }, [dispatch, currentExam, currentScore, APExams, userAPExams]);

  // Fetch saved AP exams and save to store
  useEffect(() => {
    trpc.transferCredits.getSavedAPExams.query().then((savedExams) => {
      for (const exam of savedExams) {
        dispatch(addUserAPExam(exam));
      }
      setExamsLoading(true);
    });
  }, [dispatch]);

  const APSelectOptions: APExamOption[] = APExams.map((exam) => ({
    value: exam,
    label: exam.fullName,
  }));

  return (
    <MenuSection title="AP Exam Credits">
      <SectionDescription>
        Enter the names of AP Exams that you&rsquo;ve taken to clear course prerequisites.
      </SectionDescription>
      {userAPExams.map((exam) => (
        <APCreditMenuTile key={exam.examName} examName={exam.examName} userScore={exam.score} userUnits={exam.units} />
      ))}
      <div className="input">
        <div className="exam-input">
          <Select
            value={APSelectOptions.find((opt) => opt.label === currentExam) ?? null}
            options={APSelectOptions}
            isSearchable
            onChange={(selectedOption) => setCurrentExam(selectedOption?.label || null)}
            placeholder="Add an AP Exam..."
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
        <div className="score-input">
          <Select
            value={currentScore ? { value: currentScore, label: currentScore.toString() } : null}
            options={[
              { value: 1, label: '1' },
              { value: 2, label: '2' },
              { value: 3, label: '3' },
              { value: 4, label: '4' },
              { value: 5, label: '5' },
            ]}
            onChange={(selectedOption) => setCurrentScore(selectedOption?.value || null)}
            placeholder="Score"
            theme={(t) => comboboxTheme(t, isDark)}
          />
        </div>
      </div>
    </MenuSection>
  );
};

export default APExamsSection;
