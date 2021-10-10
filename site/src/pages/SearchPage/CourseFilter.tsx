import React, { FC, MouseEvent, useState } from 'react';
import { Accordion, Menu, Icon, AccordionTitleProps } from 'semantic-ui-react';
import { Form } from 'react-bootstrap';
import './Filter.scss';
import CustomFilterOption from './CustomFilterOption';
import { RefinementListFilter, Select, TermQuery, CheckboxFilter, MenuFilter, ItemProps, ListProps, CheckboxItemList } from 'searchkit';

const ListOption = (props: ListProps) => {
  let label = props.items[0].key;
  let count = props.items[0].doc_count;
  let active = props.selectedItems.includes(label);
  const onClick = () => {
    props.toggleItem(label);
  }
  return <CustomFilterOption onClick={onClick} label={label} count={count} active={active} />
}

const GEForm = (
  <div>
    <CheckboxFilter listComponent={ListOption} title='ge-Ia' id='ge-Ia' label='GE Ia: Lower Division Writing' filter={TermQuery('ge_list.keyword', 'GE Ia: Lower Division Writing')} />
    <CheckboxFilter listComponent={ListOption} title='ge-Ib' id='ge-Ib' label='GE Ib: Upper Division Writing' filter={TermQuery('ge_list.keyword', 'GE Ib: Upper Division Writing')} />
    <CheckboxFilter listComponent={ListOption} title='ge-II' id='ge-II' label='GE II: Science and Technology' filter={TermQuery('ge_list.keyword', 'GE II: Science and Technology')} />
    <CheckboxFilter listComponent={ListOption} title='ge-III' id='ge-III' label='GE III: Social & Behavioral Sciences' filter={TermQuery('ge_list.keyword', 'GE III: Social & Behavioral Sciences')} />
    <CheckboxFilter listComponent={ListOption} title='ge-IV' id='ge-IV' label='GE IV: Arts and Humanities' filter={TermQuery('ge_list.keyword', 'GE IV: Arts and Humanities')} />
    <CheckboxFilter listComponent={ListOption} title='ge-Va' id='ge-Va' label='GE Va: Quantitative Literacy' filter={TermQuery('ge_list.keyword', 'GE Va: Quantitative Literacy')} />
    <CheckboxFilter listComponent={ListOption} title='ge-Vb' id='ge-Vb' label='GE Vb: Formal Reasoning' filter={TermQuery('ge_list.keyword', 'GE Vb: Formal Reasoning')} />
    <CheckboxFilter listComponent={ListOption} title='ge-VI' id='ge-VI' label='GE VI: Language Other Than English' filter={TermQuery('ge_list.keyword', 'GE VI: Language Other Than English')} />
    <CheckboxFilter listComponent={ListOption} title='ge-VII' id='ge-VII' label='GE VII: Multicultural Studies' filter={TermQuery('ge_list.keyword', 'GE VII: Multicultural Studies')} />
    <CheckboxFilter listComponent={ListOption} title='ge-VII' id='ge-VIII' label='GE VIII: International/Global Issues' filter={TermQuery('ge_list.keyword', 'GE VIII: International/Global Issues')} />
  </div>
)

const CourseLevelForm = (
  <div>
    <CheckboxFilter listComponent={ListOption} title='course-level-lower' id='course-level-lower' label='Lower Division (1-99)' filter={TermQuery('course_level.keyword', 'Lower Division (1-99)')} />
    <CheckboxFilter listComponent={ListOption} title='course-level-upper' id='course-level-upper' label='Upper Division (100-199)' filter={TermQuery('course_level.keyword', 'Upper Division (100-199)')} />
    <CheckboxFilter listComponent={ListOption} title='course-level-grad' id='course-level-grad' label='Graduate/Professional Only (200+)' filter={TermQuery('course_level.keyword', 'Graduate/Professional Only (200+)')} />
  </div>
)

const CourseFilter: FC = () => {
  const [activeIndex, setActiveIndex] = useState(0);
  const [departmentSearch, setDepartmentSearch] = useState('');
  const [schoolSearch, setSchoolSearch] = useState('');

  const DepartmentOption = (props: ItemProps) => {
    if (departmentSearch.length != 0 && !props.label.startsWith(departmentSearch)) {
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

  const handleClick = (e: MouseEvent, titleProps: AccordionTitleProps) => {
    const { index } = titleProps
    const newIndex = activeIndex === index ? -1 : index
    setActiveIndex(newIndex as number);
  }

  return (
    <div className='filter-list-container'>
      <div style={{ overflowY: 'auto' }}>
        <Accordion>
          <Menu.Item className='filter-item'>
            <Accordion.Title active={activeIndex === 0}
              content='Term Offered'
              index={0}
              onClick={handleClick}
            >
            </Accordion.Title>
            <Accordion.Content active={activeIndex === 0} content={<MenuFilter
              title="Terms" field='terms.keyword' id='terms'
              orderKey='_term' orderDirection='desc' listComponent={Select} />} />
          </Menu.Item>

          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 1}
              content='General Education'
              index={1}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 1} content={GEForm} />
          </Menu.Item>

          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 4}
              content='Department'
              index={4}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 4}>
              <Form.Control className='filter-search' type="text" placeholder="Search for a department" value={departmentSearch}
                onChange={e => setDepartmentSearch(e.target.value.toUpperCase())} />
              <RefinementListFilter id='depts' field='department.keyword' operator='OR' size={200} title='' itemComponent={DepartmentOption} />
            </Accordion.Content>
          </Menu.Item>

          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 2}
              content='Course Level'
              index={2}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 2} content={CourseLevelForm} />
          </Menu.Item>

          <Menu.Item className='filter-item'>
            <Accordion.Title
              active={activeIndex === 3}
              content='School'
              index={3}
              onClick={handleClick}
            />
            <Accordion.Content active={activeIndex === 3} >
              <Form.Control className='filter-search' type="text" placeholder="Search for a school" value={schoolSearch}
                onChange={e => setSchoolSearch(e.target.value)} />
              <RefinementListFilter
                id='school' field='school.keyword' operator='OR' title='' itemComponent={SchoolOption} />
            </Accordion.Content>
          </Menu.Item>
        </Accordion>
      </div>
    </div>
  )
}

export default CourseFilter;