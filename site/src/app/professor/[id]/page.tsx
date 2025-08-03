import ProfessorPage from '../ProfessorPage';

interface ProfessorPageParams {
  params: Promise<{ id: string }>;
}
const Page = async ({ params }: ProfessorPageParams) => {
  return <ProfessorPage netid={decodeURIComponent((await params).id)} />;
};
export default Page;
