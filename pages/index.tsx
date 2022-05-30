import Typography from '@material-ui/core/Typography';
import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';
import Link from '../components/Link';

const HomePage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Typography variant="h3" component="h1" align="center">
      MTG CB Limited Edition Beta - Build v0.38.0 👷‍♂️
    </Typography>
    <Typography paragraph align="center">
      <em>Last updated: 2022-05-30</em>
    </Typography>
    <br />
    <br />
    <Typography variant="h5" component="h2">
      Introduction
    </Typography>
    <br />
    <Typography paragraph>
      Thank you for joining the Limited Edition Beta! This is a very early look at the beta build, open to everyone, which is updated (and
      broken 🚧) regularly. Your feedback is highly valued and appreciated!
    </Typography>
    <Typography variant="h5" component="h2">
      Important
    </Typography>
    <Typography paragraph component="div">
      <ul>
        <li>
          <strong>
            <em>This beta is a work in progress!</em>
          </strong>{' '}
          All the features from the old MTG CB are still being brought over one at a time, along with improved interfaces.
        </li>
        <li>Changes made to your collection here are also reflected on the old MTG CB.</li>
        <li>If something is missing, it's most likely still being worked on.</li>
        <li>We'll soon move on to adding tokens, custom collection goals, and foreign cards.</li>
        <li>Best experienced on desktop, but mobile optimizations are in progress.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      Getting Started
    </Typography>
    <Typography paragraph component="div">
      <ul>
        <li>
          Check out the <u>Browse</u> section on the left, even if you're not logged in, to browse every card and set in Magic with a brand
          new set of filters and viewing options.
        </li>
        <li>You can also log into your existing MTG CB account by clicking the menu on the top-right.</li>
        <li>
          This will unveil the Collection menu option (which will let you edit <em>the same collection</em> as the old MTG CB site), so
          you're welcome to bounce between both experiences.
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      Feedback
    </Typography>
    <Typography paragraph component="div">
      <ul>
        <li>
          Feel free to reach out to me via{' '}
          <Link href="https://www.facebook.com/mtgcollectionbuilder" target="_blank">
            Facebook
          </Link>
          ,{' '}
          <Link href="mailto:brian@mtgcb.com" target="_blank">
            email
          </Link>
          , or Discord if you're a Patron.
        </li>
        <li>
          If you want to support development of MTG CB and get access to our Discord, feel free to{' '}
          <Link href="https://patreon.com/mtgcollectionbuilder" target="_blank">
            Support Me On Patreon
          </Link>{' '}
          -- there are cool stickers, magnets, and postcards you can get as rewards at specific tiers!
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      Coming Up Next
    </Typography>
    <Typography paragraph component="div">
      <ul>
        <li>Collection-based filters and sorting</li>
        <li>
          Database data fixes -- sets missing release dates, cards missing sets and collector numbers, better naming for variants, etc.
        </li>
        <li>Table improvements, including possibly embedded exporters</li>
        <li>The Great Subset Split™ and subset interfaces</li>
        <li>Bug fixes</li>
        <li>Tokens</li>
        <li>Custom collection goals</li>
        <li>Foreign cards</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      Roadmap
    </Typography>
    <Typography paragraph component="div">
      <ul>
        <li>
          Limited Edition Alpha (Complete): Initial feature parity progress with the current MTG CB. This is being moved one piece at a
          time.
        </li>
        <li>
          ⏳ Limited Edition Beta (In Progress): As many of the new MTG CB features that can be squeezed in here (Tokens, Custom Collection
          Goals, and Foreign Cards), although they'll be deferred if more current feature improvements are needed.
        </li>
        <li>Unlimited Edition (June): Full release of the new MTG CB.</li>
        <li>
          Combo Winter (New): I need to invest some time in upgrading the libraries used by the new MTG CB. I'll do this after the main
          release.
        </li>
        <li>Revised Edition (TBD): The v1.1 update adding a single new requested feature.</li>
        <li>
          Future Updates (TBD): Future updates will follow the same pattern of v1.2, v1.3, and so on, each adding a single new feature in
          focused iterations.
        </li>
      </ul>
    </Typography>
    This page will eventually be a snazzy homepage, but for now, you can use it to see what's new in the latest build and what's coming up
    next. Feel free to send me any feedback via email, Facebook, Discord or Patreon! <em>There will be bugs. 🐛</em>
  </ResponsiveContainer>
);

export default HomePage;
