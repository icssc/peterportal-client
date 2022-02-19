import React, { FC, ChangeEvent, useState, useEffect } from 'react';
import Form from 'react-bootstrap/Form';
import Button from 'react-bootstrap/Button';
import './AddCoursePopup.scss';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { moveCourse, setShowAddCourse, setShowSearch } from '../../store/slices/roadmapSlice';
import { ReportData } from '../../types/types';
import { useCookies } from 'react-cookie';
import Modal from 'react-bootstrap/Modal';
import { isMobile } from 'react-device-detect';

interface AddCoursePopupProps {
}

const AddCoursePopup: FC<AddCoursePopupProps> = (props) => {
    const dispatch = useAppDispatch();
    const planner = useAppSelector(state => state.roadmap.yearPlans);
    const showForm = useAppSelector(state => state.roadmap.showAddCourse);
    const [year, setYear] = useState(-1);
    const [quarter, setQuarter] = useState(-1);
    const [validated, setValidated] = useState(false);

    const closeForm = () => {
        // close form
        dispatch(setShowAddCourse(false));
    }

    const submit = (event: React.FormEvent<HTMLFormElement>) => {
        // validate form
        const form = event.currentTarget;
        const valid = form.checkValidity();
        event.preventDefault();
        event.stopPropagation();

        // validated
        setValidated(true);

        // do not proceed if not valid
        if (valid === false) {
            return;
        }

        // add course to roadmap
        dispatch(moveCourse({
            from: {
                yearIndex: -1,
                quarterIndex: -1,
                courseIndex: -1
            },
            to: {
                yearIndex: year,
                quarterIndex: quarter,
                courseIndex: 0
            }
        }));

        closeForm();
    }

    function capitalizeFirstLetter(string: string) {
        return string.charAt(0).toUpperCase() + string.slice(1);
    }

    const addCourseForm = (
        <Form noValidate validated={validated} onSubmit={submit}>
            <h2 className='add-course-form-header'>Add Course</h2>
            <p>Where do you want to add this course?</p>
            <Form.Group controlId='year'>
                <Form.Label>Year</Form.Label>
                <Form.Control as="select" name='year' id='year' required
                    onChange={(e) => setYear(parseInt(e.target.value))}>
                    {/* <option disabled={true} selected value=''>Year</option> */}
                    {planner.map((plannerYear, i) => {
                        let value = plannerYear.startYear;
                        return <option key={'add-course-form-year-' + i} value={i}>{value}</option>
                    })}
                </Form.Control>
                <Form.Control.Feedback type="invalid">
                    Missing year
                </Form.Control.Feedback>
            </Form.Group>
            {
                year != -1 && <Form.Group controlId='quarter'>
                    <Form.Label>Quarter</Form.Label>
                    <Form.Control as="select" name='quarter' id='quarter' required
                        onChange={(e) => setQuarter(parseInt(e.target.value))}>
                        {/* <option disabled={true} selected value=''>Quarter</option> */}
                        {planner[year].quarters.map((plannerQuarter, i) => {
                            let value = capitalizeFirstLetter(plannerQuarter.name);
                            return <option key={'add-course-form-quarter-' + i} value={i}>{value}</option>
                        })}
                    </Form.Control>
                    <Form.Control.Feedback type="invalid">
                        Missing qurter
                    </Form.Control.Feedback>
                </Form.Group>
            }
            <div className='d-flex justify-content-end'>
                <Button className='py-2 px-4 mr-3' variant="outline-secondary" size={isMobile ? 'sm' : undefined} onClick={closeForm}>Cancel</Button>
                <Button className='py-2 px-4' type="submit" variant="secondary" size={isMobile ? 'sm' : undefined}>Submit</Button>
            </div>
        </Form >
    );

    return (
        <Modal show={showForm} animation={false} onHide={closeForm} centered>
            <div className='add-course-form'>
                {addCourseForm}
            </div>
        </Modal>
    );
}

export default AddCoursePopup;