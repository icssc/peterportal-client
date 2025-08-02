import { FC } from 'react';
import ProfessorPage from '../../../pages/ProfessorPage';

interface ProfessorPageParams {
  params: { id: string };
}
const Page: FC<ProfessorPageParams> = ({ params }) => {
  return <ProfessorPage netid={decodeURIComponent(params.id)} />;
};
export default Page;
