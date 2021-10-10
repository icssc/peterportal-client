import React, { FC, MouseEvent, useState } from 'react';
import { Accordion, Menu, AccordionTitleProps } from 'semantic-ui-react';
import './Filter.scss';
import { RefinementListFilter, ItemProps } from 'searchkit';
import CustomFilterOption from './CustomFilterOption';
import { Form } from 'react-bootstrap';

const ProfessorFilter: FC = () => {
  const [activeIndex, setActiveIndex] = useState(0);
  const [courseSearch, setCourseSearch] = useState('');
  const [departmentSearch, setDepartmentSearch] = useState('');
  const [schoolSearch, setSchoolSearch] = useState('');

  const handleClick = (e: MouseEvent, titleProps: AccordionTitleProps) => {
    const { index } = titleProps
    const newIndex = activeIndex === index ? -1 : index
    setActiveIndex(newIndex as number);
  }

  const CourseOption = (props: ItemProps) => {
    if (courseSearch.length != 0 && !props.label.toUpperCase().includes(courseSearch.toUpperCase())) {
      return <></>;
    }
    return <CustomFilterOption onClick={props.onClick} label={props.label} count={props.count} active={props.active} />
  }

  const DepartmentOption = (props: ItemProps) => {
    if (departmentSearch.length != 0 && !props.label.toUpperCase().includes(departmentSearch.toUpperCase())) {
      return <></>;
    }
    return <CustomFilterOption onClick={props.onClick} label={props.label} count={props.count} active={props.active} />
  }

  const SchoolOption = (props: ItemProps) => {
    if (schoolSearch.length != 0 && !props.label.toUpperCase().includes(schoolSearch.toUpperCase())) {
      return <></>;
    }
    return <CustomFilterOption onClick={props.onClick} label={props.label} count={props.count} active={props.active} />
  }

  return (
    <div className='filter-list-container'>
      <a href='https://forms.gle/qAhCng7Ygua7SZ358'><h5 style={{ margin: 0 }}><span role='img' aria-label='thinking face'>🤔</span> Can't find your professor?</h5></a>
      <div style={{ overflowY: 'auto' }}>
        <Accordion vertical>
          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 0}
              content='School'
              index={0}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 0} >
              <Form.Control className='filter-search' type="text" placeholder="Search for a school" value={schoolSearch}
                onChange={e => setSchoolSearch(e.target.value)} />
              <RefinementListFilter
                id='schools' field='schools.keyword' operator='OR' title='' itemComponent={SchoolOption} />
            </Accordion.Content>
          </Menu.Item>

          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 1}
              content='Department'
              index={1}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 1}>
              <Form.Control className='filter-search' type="text" placeholder="Search for a department" value={departmentSearch}
                onChange={e => setDepartmentSearch(e.target.value)} />
              <RefinementListFilter id='depts' field='department.keyword' operator='OR' size={200} title='' itemComponent={DepartmentOption} />
            </Accordion.Content>
          </Menu.Item>


          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 2}
              content='Course'
              index={2}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 2}>
              <Form.Control className='filter-search' type="text" placeholder="Search for a course" value={courseSearch}
                onChange={e => setCourseSearch(e.target.value)} />
              <RefinementListFilter id='courses' field='course_history.keyword' operator='OR' size={200} title='' itemComponent={CourseOption} />
            </Accordion.Content>
          </Menu.Item>
        </Accordion>
      </div>
    </div>
  );
}

export default ProfessorFilter;
