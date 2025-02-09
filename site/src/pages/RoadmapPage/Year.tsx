import { FC, useContext, useRef, useState } from 'react';
import './Year.scss';
import { Button, Popover, OverlayTrigger } from 'react-bootstrap';
import { CaretRightFill, CaretDownFill, ThreeDots } from 'react-bootstrap-icons';
import Quarter from './Quarter';
import { useAppDispatch } from '../../store/hooks';
import { addQuarter, editYear, editName, deleteYear, clearYear, deleteQuarter } from '../../store/slices/roadmapSlice';
import { pluralize } from '../../helpers/util';

import { PlannerYearData } from '../../types/types';
import ThemeContext from '../../style/theme-context';
import YearModal from './YearModal';

interface YearProps {
  yearIndex: number;
  data: PlannerYearData;
}

const Year: FC<YearProps> = ({ yearIndex, data }) => {
  const dispatch = useAppDispatch();
  const [showContent, setShowContent] = useState(true);
  const [show, setShow] = useState(false);
  const [showEditYear, setShowEditYear] = useState(false);
  const [placeholderYear, setPlaceholderYear] = useState(data.startYear);
  const [placeholderName, setPlaceholderName] = useState(data.name);
  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';
  const yearContainerRef = useRef<HTMLDivElement>(null);

  const handleEditYearClick = (/* event: React.MouseEvent */) => {
    setPlaceholderYear(data.startYear); // set default year to current year
    setPlaceholderName(data.name);
    setShowEditYear(true);
    setShow(false); // when opening the modal, close the options menu
  };

  const calculateYearStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    data.quarters.forEach((quarter) => {
      quarter.courses.forEach((course) => {
        unitCount += course.minUnits;
        courseCount += 1;
      });
    });
    return { unitCount, courseCount };
  };

  const { unitCount, courseCount } = calculateYearStats();

  const editYearOverlay = (
    <Popover id={`year-menu-${yearIndex}`} className="year-settings-popover">
      <Popover.Content>
        <div>
          <Button onClick={handleEditYearClick} variant={buttonVariant} className="year-settings-btn">
            Edit Year
          </Button>
          <Button
            variant={buttonVariant}
            className="year-settings-btn"
            id="clear-btn"
            onClick={() => {
              dispatch(
                clearYear({
                  yearIndex: yearIndex,
                }),
              );
              setShow(false);
            }}
          >
            Clear
          </Button>
          <Button
            variant={buttonVariant}
            className="year-settings-btn"
            id="remove-btn"
            onClick={() => {
              dispatch(
                deleteYear({
                  yearIndex: yearIndex,
                }),
              );
              setShow(false);
            }}
          >
            Remove
          </Button>
        </div>
      </Popover.Content>
    </Popover>
  );

  return (
    <div className="year" ref={yearContainerRef}>
      <div className="yearTitleBar">
        <Button
          variant="link"
          className="year-accordion"
          onClick={() => {
            setShowContent(!showContent);
          }}
        >
          <span className="year-accordion-title">
            <span id="year-title">
              {showContent ? <CaretDownFill className="caret-icon" /> : <CaretRightFill className="caret-icon" />}
              {data.name ? (
                <span id="year-number">{data.name} </span>
              ) : (
                <span id="year-number">Year {yearIndex + 1} </span>
              )}
              <span id="year-range">
                ({data.startYear} - {data.startYear + 1})
              </span>
            </span>
            <span id="year-stats">
              <span id="course-count">{courseCount}</span> course{pluralize(courseCount)},{' '}
              <span id="unit-count">{unitCount}</span> unit{pluralize(unitCount)}
            </span>
          </span>
        </Button>

        <OverlayTrigger
          trigger="click"
          overlay={editYearOverlay}
          rootClose
          onToggle={setShow}
          show={show}
          placement="bottom"
          container={yearContainerRef}
        >
          {({ ref, ...triggerHandler }) => (
            <button ref={ref} {...triggerHandler} className="year-edit-btn">
              <ThreeDots />
            </button>
          )}
        </OverlayTrigger>
        <YearModal
          key={`edit-year-${placeholderYear}-${placeholderName}`}
          placeholderName={placeholderName ?? 'Year ' + (yearIndex + 1)}
          placeholderYear={placeholderYear}
          show={showEditYear}
          setShow={setShowEditYear}
          saveHandler={({ startYear, name, quarters }) => {
            setShowEditYear(false);
            if (startYear !== data.startYear) {
              setPlaceholderYear(startYear);
              dispatch(editYear({ startYear, index: yearIndex }));
            }
            if (name !== data.name) {
              setPlaceholderName(name);
              dispatch(editName({ name, index: yearIndex }));
            }
            const existing = data.quarters;
            let removed = 0;
            existing.forEach(({ name }, index) => {
              const remove = !quarters.find((q) => q.name === name);
              // Increment removed because the index of the quarters will change
              if (remove) dispatch(deleteQuarter({ yearIndex, quarterIndex: index - removed++ }));
            });
            const addQuarters = quarters.filter(({ name }) => !existing.find((q) => q.name === name));
            for (const { name } of addQuarters) {
              dispatch(addQuarter({ startYear, quarterData: { name, courses: [] } }));
            }
          }}
          currentQuarters={data.quarters.map((q) => q.name)}
          type="edit"
        />
      </div>
      {showContent && (
        <>
          <hr />
          <div className={`year-accordion-content year-accordion-content-length-${data.quarters.length}`}>
            {data.quarters.map((quarter, quarterIndex) => {
              return (
                <Quarter
                  key={`year-quarter-${quarterIndex}`}
                  year={data.startYear + (quarterIndex == 0 ? 0 : 1)}
                  yearIndex={yearIndex}
                  quarterIndex={quarterIndex}
                  data={quarter}
                />
              );
            })}
          </div>
        </>
      )}
    </div>
  );
};

export default Year;
