import { useRouter } from 'next/router';
import { ResponsiveContainer } from '../../../../components/layout/ResponsiveContainer';
import { Set } from '../../../../features/collections/sets/Set';

const SetPage: React.FC = () => {
  const router = useRouter();
  const { userId, setSlug } = router.query;
  const id = userId as string;
  const slug = setSlug as string;

  return (
    <ResponsiveContainer maxWidth="xl">
      <Set setSlug={slug} userId={id} />
    </ResponsiveContainer>
  );
};

export default SetPage;
